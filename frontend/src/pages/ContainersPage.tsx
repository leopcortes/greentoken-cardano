import { useEffect, useState } from 'react';
import { toast } from 'sonner';
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
import { SearchableSelect } from '@/components/ui/searchable-select';
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from '@/components/ui/tooltip';
import { useSortable } from '@/hooks/useSortable';
import { truncateMiddle } from '@/lib/truncate';
import { getContainers, createContainer, compactContainer, getUsers, type Container, type User } from '@/services/api';
import { CONTAINER_STATUS_LABELS, t } from '@/lib/labels';

const STATUS_COLORS: Record<string, string> = {
  active: 'bg-green-100 text-green-800',
  full: 'bg-red-100 text-red-800',
  compacted: 'bg-yellow-100 text-yellow-800',
  in_route: 'bg-blue-100 text-blue-800',
  maintenance: 'bg-gray-100 text-gray-800',
};

export function ContainersPage() {
  const [containers, setContainers] = useState<Container[]>([]);
  const [usersMap, setUsersMap] = useState<Record<string, User>>({});
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [dialogOpen, setDialogOpen] = useState(false);
  const [submitting, setSubmitting] = useState(false);
  const [compactingId, setCompactingId] = useState<string | null>(null);

  // Form state
  const [formOwnerId, setFormOwnerId] = useState('');
  const [ownerError, setOwnerError] = useState('');
  const [formName, setFormName] = useState('');
  const [nameError, setNameError] = useState('');
  const [formLocation, setFormLocation] = useState('');
  const [formLatitude, setFormLatitude] = useState('');
  const [formLongitude, setFormLongitude] = useState('');
  const [formCapacity, setFormCapacity] = useState('100');
  const [capacityError, setCapacityError] = useState('');

  // Owner select options
  const [ownerOptions, setOwnerOptions] = useState<User[]>([]);

  const { sorted, sortKey, sortDir, toggleSort } = useSortable<Container>(containers);

  const fetchContainers = async () => {
    try {
      setLoading(true);
      const [containersData, usersData] = await Promise.all([getContainers({ status: 'all' }), getUsers()]);
      setContainers(containersData);
      const map: Record<string, User> = {};
      for (const u of usersData) map[u.id] = u;
      setUsersMap(map);
      setError('');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao carregar containers');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => { fetchContainers(); }, []);

  const openCreateDialog = async () => {
    try {
      const usersData = await getUsers('owner');
      setOwnerOptions(usersData);
      setFormOwnerId('');
      setFormName('');
      setFormLocation('');
      setFormLatitude('');
      setFormLongitude('');
      setFormCapacity('100');
      setOwnerError('');
      setNameError('');
      setCapacityError('');
      setDialogOpen(true);
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Erro ao carregar owners', { duration: 10000 });
    }
  };

  const handleCreate = async (e: React.FormEvent) => {
    e.preventDefault();

    let hasError = false;

    if (!formOwnerId) {
      setOwnerError('Selecione um dono.');
      hasError = true;
    } else {
      setOwnerError('');
    }

    if (!formName.trim()) {
      setNameError('O nome é obrigatório.');
      hasError = true;
    } else {
      setNameError('');
    }

    if (!formCapacity.trim() || Number(formCapacity) <= 0) {
      setCapacityError('A capacidade deve ser maior que zero.');
      hasError = true;
    } else {
      setCapacityError('');
    }

    if (hasError) return;

    setSubmitting(true);
    try {
      await createContainer({
        owner_id: formOwnerId,
        name: formName,
        ...(formLocation ? { location_name: formLocation } : {}),
        ...(formLatitude ? { latitude: Number(formLatitude) } : {}),
        ...(formLongitude ? { longitude: Number(formLongitude) } : {}),
        capacity_liters: Number(formCapacity),
      });

      toast.success(`Container "${formName}" criado com sucesso.`, { duration: 5000 });
      setDialogOpen(false);
      fetchContainers();
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Erro ao criar container', { duration: 10000 });
    } finally {
      setSubmitting(false);
    }
  };

  const handleCompact = async (container: Container) => {
    setCompactingId(container.id);
    const toastId = toast.loading(<>Compactando garrafas de "{container.name}".<br />Aguardando transações na blockchain...</>, { duration: 7500 });
    try {
      const result = await compactContainer(container.id);
      toast.success(result.message, { id: toastId, duration: 5000 });
      fetchContainers();
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Erro ao compactar', { id: toastId, duration: 10000 });
    } finally {
      setCompactingId(null);
    }
  };

  const fillPercent = (c: Container) =>
    c.capacity_liters > 0 ? Math.round((c.current_volume_liters / c.capacity_liters) * 100) : 0;

  const formatCoord = (lat: number | null, lng: number | null) => {
    if (lat == null || lng == null) return '-';
    return `${lat.toFixed(5)}, ${lng.toFixed(5)}`;
  };

  const SH = (label: string, key: keyof Container) => (
    <SortableHeader label={label} sortKey={key as string} currentKey={sortKey as string | null} direction={sortDir} onSort={() => toggleSort(key)} />
  );

  return (
    <div className="space-y-3">
      <div className="flex items-center justify-between">
        <h2 className="text-lg font-semibold">Containers</h2>
        <div className="flex gap-2">
          <Button variant="outline" size="sm" className='bg-gray-100 text-gray-800' onClick={fetchContainers}>Atualizar</Button>
          <Dialog open={dialogOpen} onOpenChange={setDialogOpen}>
            <DialogTrigger asChild>
              <Button size="sm" onClick={openCreateDialog}>+ Novo Container</Button>
            </DialogTrigger>
            <DialogContent className="overflow-visible">
              <DialogHeader>
                <DialogTitle>Criar Container</DialogTitle>
              </DialogHeader>
              <form onSubmit={handleCreate} className="space-y-4">
                <div className="space-y-2">
                  <Label>
                    Dono (Owner)<span className="text-red-500">*</span>
                  </Label>
                  <SearchableSelect
                    options={ownerOptions.map(u => ({
                      value: u.id,
                      label: u.name,
                      detail: u.email,
                    }))}
                    value={formOwnerId}
                    onValueChange={v => { setFormOwnerId(v); if (ownerError) setOwnerError(''); }}
                    placeholder="Selecione um owner..."
                    searchPlaceholder="Buscar por nome ou email..."
                    error={!!ownerError}
                  />
                  {ownerError && (
                    <p className="text-sm font-medium text-red-500">{ownerError}</p>
                  )}
                </div>

                <div className="space-y-2">
                  <Label htmlFor="container-name">
                    Nome<span className="text-red-500">*</span>
                  </Label>
                  <Input
                    id="container-name"
                    value={formName}
                    onChange={e => {
                      setFormName(e.target.value);
                      if (nameError) setNameError('');
                    }}
                    placeholder="Ponto de Coleta Centro"
                    className={nameError ? 'border-red-500 focus-visible:ring-red-500' : ''}
                  />
                  {nameError && (
                    <p className="text-sm font-medium text-red-500">{nameError}</p>
                  )}
                </div>

                <div className="space-y-2">
                  <Label htmlFor="container-location">Local</Label>
                  <Input id="container-location" value={formLocation} onChange={e => setFormLocation(e.target.value)} placeholder="Asa Norte, Brasilia" />
                </div>

                <div className="grid grid-cols-2 gap-3">
                  <div className="space-y-2">
                    <Label htmlFor="container-lat">Latitude</Label>
                    <Input
                      id="container-lat"
                      type="number"
                      step="any"
                      value={formLatitude}
                      onChange={e => setFormLatitude(e.target.value)}
                      placeholder="-15.7935"
                    />
                  </div>
                  <div className="space-y-2">
                    <Label htmlFor="container-lng">Longitude</Label>
                    <Input
                      id="container-lng"
                      type="number"
                      step="any"
                      value={formLongitude}
                      onChange={e => setFormLongitude(e.target.value)}
                      placeholder="-47.8828"
                    />
                  </div>
                </div>

                <div className="space-y-2">
                  <Label htmlFor="container-capacity">
                    Capacidade (litros)<span className="text-red-500">*</span>
                  </Label>
                  <Input
                    id="container-capacity"
                    type="number"
                    step="0.1"
                    value={formCapacity}
                    onChange={e => {
                      setFormCapacity(e.target.value);
                      if (capacityError) setCapacityError('');
                    }}
                    className={capacityError ? 'border-red-500 focus-visible:ring-red-500' : ''}
                  />
                  {capacityError && (
                    <p className="text-sm font-medium text-red-500">{capacityError}</p>
                  )}
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
        <CardContent className="px-4 pb-2 pt-0">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>{SH('Descrição', 'name')}</TableHead>
                <TableHead>{SH('Local', 'location_name')}</TableHead>
                <TableHead>Coordenadas</TableHead>
                <TableHead>{SH('Responsável', 'owner_id')}</TableHead>
                <TableHead>{SH('Status', 'status')}</TableHead>
                <TableHead>{SH('Volume', 'current_volume_liters')}</TableHead>
                <TableHead>{SH('Atualizado em', 'last_updated')}</TableHead>
                <TableHead>Ações</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {sorted.map(c => {
                const user = usersMap[c.owner_id];
                const percent = fillPercent(c);
                const canCompact = (c.status === 'active' || c.status === 'full') && percent >= 90;
                const isCompacting = compactingId === c.id;
                return (
                <TableRow key={c.id}>
                  <TableCell className="font-medium text-sm">
                    <div className="text-sm font-medium leading-tight">{c.name}</div>
                    <div className="flex items-center gap-0.5 mt-0.5">
                      <span className="font-mono text-[11px] text-muted-foreground hidden 2xl:block">{truncateMiddle(c.id, 30, 8)}</span>
                      <span className="font-mono text-[11px] text-muted-foreground block 2xl:hidden">{truncateMiddle(c.id, 10, 5)}</span>
                      <CopyButton className='ml-1' value={c.id} />
                    </div>
                  </TableCell>
                  <TableCell className="text-sm">{c.location_name || '-'}</TableCell>
                  <TableCell className="text-xs font-mono text-muted-foreground whitespace-nowrap">
                    {formatCoord(c.latitude, c.longitude)}
                  </TableCell>
                  <TableCell>
                    <div className="text-sm font-medium leading-tight">{user.name}</div>
                    <div className="flex items-center gap-0.5 mt-0.5">
                      <span className="font-mono text-[11px] text-muted-foreground hidden 2xl:block">{truncateMiddle(c.owner_id, 30, 8)}</span>
                      <span className="font-mono text-[11px] text-muted-foreground block 2xl:hidden">{truncateMiddle(c.owner_id, 10, 5)}</span>
                      <CopyButton className='ml-1' value={c.owner_id} />
                    </div>
                  </TableCell>
                  <TableCell>
                    <Badge variant="secondary" className={STATUS_COLORS[c.status] || ''}>
                      {t(CONTAINER_STATUS_LABELS, c.status)}
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
                        {Number(c.current_volume_liters).toFixed(2)}/{Number(c.capacity_liters).toFixed(2)}L
                      </span>
                    </div>
                  </TableCell>
                  <TableCell className="text-xs text-muted-foreground whitespace-nowrap">
                    {new Date(c.last_updated).toLocaleDateString('pt-BR')}
                  </TableCell>
                  <TableCell>
                    <TooltipProvider>
                      <Tooltip>
                        <TooltipTrigger asChild>
                          <span className="inline-block">
                            <Button
                              variant="outline"
                              size="sm"
                              className={`h-7 text-xs px-2 border-0 ${
                                isCompacting ? 'bg-yellow-200 text-yellow-800 hover:bg-yellow-200'
                                  : canCompact ? 'bg-orange-200 text-orange-800 hover:bg-orange-200'
                                    : 'bg-gray-200 text-gray-500 hover:bg-gray-200'
                              }`}
                              disabled={isCompacting || !canCompact}
                              onClick={() => handleCompact(c)}
                            >
                              {isCompacting ? 'Compactando...' : 'Compactar'}
                            </Button>
                          </span>
                        </TooltipTrigger>

                        {!canCompact && !isCompacting && (
                          <TooltipContent className="bg-white text-black border border-gray-200" side="right">
                            <p>
                              {c.status === 'compacted'
                                ? 'Container já foi compactado. Pronto para coleta.'
                                : c.status !== 'active' && c.status !== 'full'
                                  ? `Container com status "${t(CONTAINER_STATUS_LABELS, c.status)}" não pode ser compactado.`
                                  : 'O container deve estar com pelo menos 90% da capacidade ocupada para compactar.'}
                            </p>
                          </TooltipContent>
                        )}
                      </Tooltip>
                    </TooltipProvider>
                  </TableCell>
                </TableRow>
                );
              })}
              {!loading && containers.length === 0 && (
                <TableRow>
                  <TableCell colSpan={8} className="text-center text-muted-foreground py-8">
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
