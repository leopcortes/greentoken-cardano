import { useEffect, useState } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Dialog, DialogContent, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { CopyButton } from '@/components/ui/copy-button';
import { ErrorAlert } from '@/components/ui/error-alert';
import { SortableHeader } from '@/components/ui/sortable-header';
import { useSortable } from '@/hooks/useSortable';
import { getContainers, createContainer, type Container } from '@/services/api';

const STATUS_COLORS: Record<string, string> = {
  active: 'bg-green-100 text-green-800',
  full: 'bg-red-100 text-red-800',
  in_route: 'bg-blue-100 text-blue-800',
  maintenance: 'bg-gray-100 text-gray-800',
};

export function ContainersPage() {
  const [containers, setContainers] = useState<Container[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [dialogOpen, setDialogOpen] = useState(false);
  const [formOwnerId, setFormOwnerId] = useState('');
  const [formName, setFormName] = useState('');
  const [formLocation, setFormLocation] = useState('');
  const [formCapacity, setFormCapacity] = useState('100');
  const [submitting, setSubmitting] = useState(false);

  const { sorted, sortKey, sortDir, toggleSort } = useSortable<Container>(containers);

  const fetchContainers = async () => {
    try {
      setLoading(true);
      const data = await getContainers();
      setContainers(data);
      setError('');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao carregar containers');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => { fetchContainers(); }, []);

  const handleCreate = async (e: React.FormEvent) => {
    e.preventDefault();
    setSubmitting(true);
    try {
      await createContainer({
        owner_id: formOwnerId,
        name: formName,
        ...(formLocation ? { location_name: formLocation } : {}),
        capacity_liters: Number(formCapacity),
      });
      setDialogOpen(false);
      setFormOwnerId('');
      setFormName('');
      setFormLocation('');
      setFormCapacity('100');
      fetchContainers();
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao criar container');
    } finally {
      setSubmitting(false);
    }
  };

  const fillPercent = (c: Container) =>
    c.capacity_liters > 0 ? Math.round((c.current_volume_liters / c.capacity_liters) * 100) : 0;

  const SH = (label: string, key: keyof Container) => (
    <SortableHeader label={label} sortKey={key as string} currentKey={sortKey as string | null} direction={sortDir} onSort={() => toggleSort(key)} />
  );

  return (
    <div className="space-y-3">
      <div className="flex items-center justify-between">
        <h2 className="text-lg font-semibold">Containers</h2>
        <div className="flex gap-2">
          <Button variant="outline" size="sm" onClick={fetchContainers}>Atualizar</Button>
          <Dialog open={dialogOpen} onOpenChange={setDialogOpen}>
            <DialogTrigger asChild>
              <Button size="sm">+ Novo Container</Button>
            </DialogTrigger>
            <DialogContent>
              <DialogHeader>
                <DialogTitle>Criar Container</DialogTitle>
              </DialogHeader>
              <form onSubmit={handleCreate} className="space-y-4">
                <div className="space-y-2">
                  <Label htmlFor="container-owner">ID do Dono (UUID)</Label>
                  <Input id="container-owner" value={formOwnerId} onChange={e => setFormOwnerId(e.target.value)} required placeholder="UUID do owner" />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="container-name">Nome</Label>
                  <Input id="container-name" value={formName} onChange={e => setFormName(e.target.value)} required placeholder="Ponto de Coleta Centro" />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="container-location">Local (opcional)</Label>
                  <Input id="container-location" value={formLocation} onChange={e => setFormLocation(e.target.value)} placeholder="Asa Norte, Brasilia" />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="container-capacity">Capacidade (litros)</Label>
                  <Input id="container-capacity" type="number" value={formCapacity} onChange={e => setFormCapacity(e.target.value)} required />
                </div>
                <Button type="submit" disabled={submitting} className="w-full">
                  {submitting ? 'Criando...' : 'Criar'}
                </Button>
              </form>
            </DialogContent>
          </Dialog>
        </div>
      </div>

      {error && <ErrorAlert message={error} onDismiss={() => setError('')} />}

      <Card>
        <CardHeader className="py-3 px-4">
          <CardTitle className="text-sm font-medium text-muted-foreground">
            {loading ? 'Carregando...' : `${containers.length} container(s) encontrado(s)`}
          </CardTitle>
        </CardHeader>
        <CardContent className="px-4 pb-4 pt-0">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>{SH('Nome', 'name')}</TableHead>
                <TableHead>{SH('Local', 'location_name')}</TableHead>
                <TableHead>Dono</TableHead>
                <TableHead>{SH('Status', 'status')}</TableHead>
                <TableHead>{SH('Volume', 'current_volume_liters')}</TableHead>
                <TableHead>{SH('Atualizado em', 'last_updated')}</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {sorted.map(c => (
                <TableRow key={c.id}>
                  <TableCell className="font-medium text-sm">{c.name}</TableCell>
                  <TableCell className="text-sm">{c.location_name || '-'}</TableCell>
                  <TableCell>
                    <div className="flex items-center gap-0.5">
                      <span className="font-mono text-xs">{c.owner_id}...</span>
                      <CopyButton className='ml-1' value={c.owner_id} />
                    </div>
                  </TableCell>
                  <TableCell>
                    <Badge variant="secondary" className={STATUS_COLORS[c.status] || ''}>
                      {c.status}
                    </Badge>
                  </TableCell>
                  <TableCell>
                    <div className="flex items-center gap-2">
                      <div className="h-2 w-16 rounded-full bg-muted overflow-hidden">
                        <div
                          className={`h-full rounded-full transition-all ${fillPercent(c) >= 90 ? 'bg-red-500' : 'bg-green-500'}`}
                          style={{ width: `${fillPercent(c)}%` }}
                        />
                      </div>
                      <span className="text-xs text-muted-foreground whitespace-nowrap">
                        {c.current_volume_liters}/{c.capacity_liters}L
                      </span>
                    </div>
                  </TableCell>
                  <TableCell className="text-xs text-muted-foreground whitespace-nowrap">
                    {new Date(c.last_updated).toLocaleDateString('pt-BR')}
                  </TableCell>
                </TableRow>
              ))}
              {!loading && containers.length === 0 && (
                <TableRow>
                  <TableCell colSpan={6} className="text-center text-muted-foreground py-8">
                    Nenhum container encontrado
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </CardContent>
      </Card>
    </div>
  );
}
