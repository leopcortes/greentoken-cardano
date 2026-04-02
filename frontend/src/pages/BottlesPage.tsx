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
import { getBottles, getUsers, createBottle, advanceBottle, type Bottle, type User } from '@/services/api';
import { truncateMiddle } from '@/lib/truncate';

const STAGE_ORDER = ['inserted', 'compacted', 'collected', 'atstation', 'shredded'];

const STAGE_COLORS: Record<string, string> = {
  inserted: 'bg-blue-100 text-blue-800',
  compacted: 'bg-yellow-100 text-yellow-800',
  collected: 'bg-orange-100 text-orange-800',
  atstation: 'bg-purple-100 text-purple-800',
  shredded: 'bg-green-100 text-green-800',
};

export function BottlesPage() {
  const [bottles, setBottles] = useState<Bottle[]>([]);
  const [usersMap, setUsersMap] = useState<Record<string, User>>({});
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [dialogOpen, setDialogOpen] = useState(false);
  const [formBottleId, setFormBottleId] = useState('');
  const [formUserId, setFormUserId] = useState('');
  const [formContainerId, setFormContainerId] = useState('');
  const [submitting, setSubmitting] = useState(false);

  const { sorted, sortKey, sortDir, toggleSort } = useSortable<Bottle>(bottles);

  const fetchBottles = async () => {
    try {
      setLoading(true);
      const [bottlesData, usersData] = await Promise.all([getBottles(), getUsers()]);
      setBottles(bottlesData);
      const map: Record<string, User> = {};
      for (const u of usersData) map[u.id] = u;
      setUsersMap(map);
      setError('');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao carregar garrafas');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => { fetchBottles(); }, []);

  const handleCreate = async (e: React.FormEvent) => {
    e.preventDefault();
    setSubmitting(true);
    try {
      await createBottle({
        bottle_id: formBottleId,
        user_id: formUserId,
        ...(formContainerId ? { container_id: formContainerId } : {}),
      });
      setDialogOpen(false);
      setFormBottleId('');
      setFormUserId('');
      setFormContainerId('');
      fetchBottles();
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao criar garrafa');
    } finally {
      setSubmitting(false);
    }
  };

  const handleAdvance = async (bottle: Bottle) => {
    const next = nextStage(bottle.current_stage);
    if (!next) return;
    try {
      await advanceBottle(bottle.id, next);
      fetchBottles();
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao avançar estágio');
    }
  };

  const nextStage = (current: string) => {
    const idx = STAGE_ORDER.indexOf(current);
    return idx >= 0 && idx < STAGE_ORDER.length - 1 ? STAGE_ORDER[idx + 1] : null;
  };

  const SH = (label: string, key: keyof Bottle) => (
    <SortableHeader label={label} sortKey={key as string} currentKey={sortKey as string | null} direction={sortDir} onSort={() => toggleSort(key)} />
  );

  return (
    <div className="space-y-3">
      <div className="flex items-center justify-between">
        <h2 className="text-lg font-semibold">Garrafas</h2>
        <div className="flex gap-2">
          <Button variant="outline" size="sm" onClick={fetchBottles}>Atualizar</Button>
          <Dialog open={dialogOpen} onOpenChange={setDialogOpen}>
            <DialogTrigger asChild>
              <Button size="sm">+ Nova Garrafa</Button>
            </DialogTrigger>
            <DialogContent>
              <DialogHeader>
                <DialogTitle>Criar Garrafa</DialogTitle>
              </DialogHeader>
              <form onSubmit={handleCreate} className="space-y-4">
                <div className="space-y-2">
                  <Label htmlFor="bottle-id">ID da Garrafa (texto)</Label>
                  <Input id="bottle-id" value={formBottleId} onChange={e => setFormBottleId(e.target.value)} required placeholder="garrafa-001" />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="bottle-user">ID do Usuário (UUID)</Label>
                  <Input id="bottle-user" value={formUserId} onChange={e => setFormUserId(e.target.value)} required placeholder="UUID do usuario" />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="bottle-container">ID do Container (opcional)</Label>
                  <Input id="bottle-container" value={formContainerId} onChange={e => setFormContainerId(e.target.value)} placeholder="UUID do container" />
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
            {loading ? 'Carregando...' : `${bottles.length} garrafa(s) encontrada(s)`}
          </CardTitle>
        </CardHeader>
        <CardContent className="px-4 pb-4 pt-0">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>{SH('Garrafa', 'bottle_id_text')}</TableHead>
                <TableHead>{SH('Usuário', 'user_id')}</TableHead>
                <TableHead>{SH('Estágio', 'current_stage')}</TableHead>
                <TableHead>Container</TableHead>
                <TableHead>UTXO</TableHead>
                <TableHead>{SH('Inserida em', 'inserted_at')}</TableHead>
                <TableHead>Ações</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {sorted.map(bottle => {
                const user = usersMap[bottle.user_id];
                return (
                  <TableRow key={bottle.id}>
                    <TableCell>
                      <div className="text-sm font-medium leading-tight">{bottle.bottle_id_text}</div>
                      <div className="flex items-center gap-0.5 mt-0.5">
                        <span className="font-mono text-[11px] text-muted-foreground">{truncateMiddle(bottle.id, 30, 10)}</span>
                        <CopyButton className='ml-1' value={bottle.id} />
                      </div>
                    </TableCell>
                    <TableCell>
                      <div className="text-sm font-medium leading-tight">{user?.name ?? '-'}</div>
                      <div className="flex items-center gap-0.5 mt-0.5">
                        <span className="font-mono text-[11px] text-muted-foreground">{truncateMiddle(bottle.user_id, 30, 10)}</span>
                        <CopyButton className='ml-1' value={bottle.user_id} />
                      </div>
                    </TableCell>
                    <TableCell>
                      <Badge variant="secondary" className={STAGE_COLORS[bottle.current_stage] || ''}>
                        {bottle.current_stage}
                      </Badge>
                    </TableCell>
                    <TableCell>
                      {bottle.container_id ? (
                        <div className="flex items-center gap-0.5">
                          <span className="font-mono text-xs">{truncateMiddle(bottle.container_id, 30, 10)}</span>
                          <CopyButton className='ml-1' value={bottle.container_id} />
                        </div>
                      ) : <span className="text-muted-foreground">-</span>}
                    </TableCell>
                    <TableCell>
                      {bottle.utxo_hash ? (
                        <div className="flex items-center gap-0.5">
                          <span className="font-mono text-xs">{truncateMiddle(bottle.utxo_hash, 50, 10)}#{bottle.utxo_index}</span>
                          <CopyButton className='ml-1' value={`${bottle.utxo_hash}#${bottle.utxo_index}`} />
                        </div>
                      ) : <span className="text-muted-foreground">-</span>}
                    </TableCell>
                    <TableCell className="text-xs text-muted-foreground whitespace-nowrap">
                      {new Date(bottle.inserted_at).toLocaleDateString('pt-BR')}
                    </TableCell>
                    <TableCell>
                      {nextStage(bottle.current_stage) ? (
                        <Button variant="outline" size="sm" className="h-7 text-xs px-2" onClick={() => handleAdvance(bottle)}>
                          → {nextStage(bottle.current_stage)}
                        </Button>
                      ) : (
                        <span className="text-xs text-muted-foreground">Completo</span>
                      )}
                    </TableCell>
                  </TableRow>
                );
              })}
              {!loading && bottles.length === 0 && (
                <TableRow>
                  <TableCell colSpan={7} className="text-center text-muted-foreground py-8">
                    Nenhuma garrafa encontrada
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
