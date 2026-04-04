import { useEffect, useRef, useState, useCallback } from 'react';
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
import { useSortable } from '@/hooks/useSortable';
import { SquareArrowRightEnter, Package, Truck, Factory, Recycle, Loader2 } from 'lucide-react';
import {
  getBottles, getUsers, getContainers, getNextBottleNumber,
  createBottle, getBottle,
  type Bottle, type User, type Container,
} from '@/services/api';
import { truncateMiddle } from '@/lib/truncate';
import { STAGE_LABELS, ROLE_LABELS, t } from '@/lib/labels';

const STAGE_COLORS: Record<string, string> = {
  inserted: 'bg-blue-100 text-blue-800',
  compacted: 'bg-yellow-100 text-yellow-800',
  collected: 'bg-orange-100 text-orange-800',
  atstation: 'bg-purple-100 text-purple-800',
  shredded: 'bg-green-100 text-green-800',
};

const STAGE_ICONS: Record<string, React.ElementType> = {
  inserted: SquareArrowRightEnter,
  compacted: Package,
  collected: Truck,
  atstation: Factory,
  shredded: Recycle,
};

export function BottlesPage() {
  const [bottles, setBottles] = useState<Bottle[]>([]);
  const [usersMap, setUsersMap] = useState<Record<string, User>>({});
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [dialogOpen, setDialogOpen] = useState(false);

  // Form state
  const [formUserId, setFormUserId] = useState('');
  const [userError, setUserError] = useState('');
  const [formContainerId, setFormContainerId] = useState('');
  const [containerError, setContainerError] = useState('');
  const [formVolume, setFormVolume] = useState('500');
  const [volumeError, setVolumeError] = useState('');
  const [submitting, setSubmitting] = useState(false);
  const [nextName, setNextName] = useState('');

  // Blockchain cooldown: after creating, poll until utxo is confirmed
  const [blockchainBusy, setBlockchainBusy] = useState(false);
  const pollRef = useRef<ReturnType<typeof setInterval> | null>(null);

  // Options for selects
  const [userOptions, setUserOptions] = useState<User[]>([]);
  const [containerOptions, setContainerOptions] = useState<Container[]>([]);

  const { sorted, sortKey, sortDir, toggleSort } = useSortable<Bottle>(bottles);

  const stopPolling = useCallback(() => {
    if (pollRef.current) {
      clearInterval(pollRef.current);
      pollRef.current = null;
    }
    setBlockchainBusy(false);
  }, []);

  const startPolling = useCallback((bottleId: string) => {
    setBlockchainBusy(true);
    pollRef.current = setInterval(async () => {
      try {
        const data = await getBottle(bottleId);
        if (data.bottle.utxo_hash) {
          stopPolling();
          fetchBottles();
          toast.success('Blockchain pronta. Você pode criar outra garrafa.');
        }
      } catch {
        // ignore polling errors
      }
    }, 5000);
  }, [stopPolling]);

  // Cleanup on unmount
  useEffect(() => () => { if (pollRef.current) clearInterval(pollRef.current); }, []);

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

  const openCreateDialog = async () => {
    try {
      const [usersData, containersData, nextNum] = await Promise.all([
        getUsers(),
        getContainers(),
        getNextBottleNumber(),
      ]);
      setUserOptions(usersData);
      setContainerOptions(containersData);
      setNextName(nextNum.next_name);
      setFormUserId('');
      setFormContainerId('');
      setFormVolume('500');
      setUserError('');
      setContainerError('');
      setVolumeError('');
      setDialogOpen(true);
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Erro ao carregar dados para formulário');
    }
  };

  const handleCreate = async (e: React.FormEvent) => {
    e.preventDefault();

    let hasError = false;

    if (!formUserId) {
      setUserError('Selecione um usuário.');
      hasError = true;
    } else {
      setUserError('');
    }

    if (!formContainerId) {
      setContainerError('Selecione um container.');
      hasError = true;
    } else {
      setContainerError('');
    }

    if (!formVolume.trim() || Number(formVolume) <= 0) {
      setVolumeError('O volume deve ser maior que zero.');
      hasError = true;
    } else if (formContainerId) {
      const selected = containerOptions.find(c => c.id === formContainerId);
      if (selected) {
        const remainingLiters = selected.capacity_liters - selected.current_volume_liters;
        const volumeLiters = Number(formVolume) / 1000;
        if (volumeLiters > remainingLiters) {
          setVolumeError(
            `Container sem espaço suficiente. Disponível: ${(remainingLiters * 1000).toFixed(1)}ml (${remainingLiters.toFixed(1)}L).`
          );
          hasError = true;
        } else {
          setVolumeError('');
        }
      }
    } else {
      setVolumeError('');
    }

    if (hasError) return;

    setSubmitting(true);
    try {
      const result = await createBottle({
        user_id: formUserId,
        container_id: formContainerId,
        volume_ml: Number(formVolume),
      });
      toast.success(`Garrafa "${result.bottle.bottle_id_text}" criada. Aguardando confirmação on-chain.`, {
        description: `tx: ${result.tx_hash}`,
      });
      setDialogOpen(false);
      startPolling(result.bottle.id);
      fetchBottles();
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Erro ao criar garrafa');
    } finally {
      setSubmitting(false);
    }
  };

  const SH = (label: string, key: keyof Bottle) => (
    <SortableHeader label={label} sortKey={key as string} currentKey={sortKey as string | null} direction={sortDir} onSort={() => toggleSort(key)} />
  );

  const containersMap: Record<string, Container> = {};
  for (const c of containerOptions) containersMap[c.id] = c;

  return (
    <div className="space-y-3">
      <div className="flex items-center justify-between">
        <h2 className="text-lg font-semibold">Garrafas</h2>
        <div className="flex gap-2">
          <Button variant="outline" size="sm" className='bg-gray-100 text-gray-800' onClick={fetchBottles}>Atualizar</Button>
          <Dialog open={dialogOpen} onOpenChange={setDialogOpen}>
            <DialogTrigger asChild>
              <Button size="sm" onClick={openCreateDialog} disabled={blockchainBusy}>
                {blockchainBusy && <Loader2 className="mr-1.5 h-3.5 w-3.5 animate-spin" />}
                {blockchainBusy ? 'Aguardando blockchain...' : '+ Nova Garrafa'}
              </Button>
            </DialogTrigger>
            <DialogContent>
              <DialogHeader>
                <DialogTitle>Criar Garrafa</DialogTitle>
              </DialogHeader>
              <form onSubmit={handleCreate} className="space-y-4">
                {nextName && (
                  <p className="text-sm text-muted-foreground">
                    Próxima garrafa: <span className="font-mono font-medium text-foreground">{nextName}</span>
                  </p>
                )}

                <div className="space-y-2">
                  <Label>
                    Usuário<span className="text-red-500">*</span>
                  </Label>
                  <SearchableSelect
                    options={userOptions.map(u => ({
                      value: u.id,
                      label: u.name,
                      detail: `${t(ROLE_LABELS, u.role)} - ${u.email}`,
                    }))}
                    value={formUserId}
                    onValueChange={v => { setFormUserId(v); if (userError) setUserError(''); }}
                    placeholder="Selecione um usuário..."
                    searchPlaceholder="Buscar por nome ou email..."
                    error={!!userError}
                  />
                  {userError && (
                    <p className="text-sm font-medium text-red-500">{userError}</p>
                  )}
                </div>

                <div className="space-y-2">
                  <Label>
                    Container<span className="text-red-500">*</span>
                  </Label>
                  <SearchableSelect
                    options={containerOptions
                      .filter(c => c.status === 'active' && c.current_volume_liters < c.capacity_liters)
                      .map(c => ({
                        value: c.id,
                        label: c.name,
                        detail: `${Number(c.current_volume_liters).toFixed(1)}/${Number(c.capacity_liters).toFixed(1)}L - ${c.location_name || 'Sem local'}`,
                      }))}
                    value={formContainerId}
                    onValueChange={v => { setFormContainerId(v); if (containerError) setContainerError(''); }}
                    placeholder="Selecione um container..."
                    searchPlaceholder="Buscar por nome ou local..."
                    error={!!containerError}
                  />
                  {containerError && (
                    <p className="text-sm font-medium text-red-500">{containerError}</p>
                  )}
                </div>

                <div className="space-y-2">
                  <Label htmlFor="bottle-volume">
                    Volume (ml)<span className="text-red-500">*</span>
                  </Label>
                  <Input
                    id="bottle-volume"
                    type="number"
                    step="0.1"
                    value={formVolume}
                    onChange={e => {
                      setFormVolume(e.target.value);
                      if (volumeError) setVolumeError('');
                    }}
                    placeholder="500"
                    className={volumeError ? 'border-red-500 focus-visible:ring-red-500' : ''}
                  />
                  {volumeError && (
                    <p className="text-sm font-medium text-red-500">{volumeError}</p>
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
            {loading ? 'Carregando...' : `${bottles.length} garrafa(s) encontrada(s)`}
          </CardTitle>
        </CardHeader>
        <CardContent className="px-4 pb-2 pt-0">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>{SH('Garrafa', 'bottle_id_text')}</TableHead>
                <TableHead>{SH('Usuário', 'user_id')}</TableHead>
                <TableHead>Localização</TableHead>
                <TableHead>{SH('Volume', 'volume_ml')}</TableHead>
                <TableHead>{SH('Estágio', 'current_stage')}</TableHead>
                <TableHead>UTXO</TableHead>
                <TableHead>{SH('Inserida em', 'inserted_at')}</TableHead>
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
                        <span className="font-mono text-[11px] text-muted-foreground hidden 2xl:block">{truncateMiddle(bottle.id, 20, 5)}</span>
                        <span className="font-mono text-[11px] text-muted-foreground block 2xl:hidden">{truncateMiddle(bottle.id, 8, 4)}</span>
                        <CopyButton className='ml-1' value={bottle.id} />
                      </div>
                    </TableCell>
                    <TableCell>
                      <div className="text-sm font-medium leading-tight">{user?.name ?? '-'}</div>
                      <div className="flex items-center gap-0.5 mt-0.5">
                        <span className="font-mono text-[11px] text-muted-foreground hidden 2xl:block">{truncateMiddle(bottle.user_id, 20, 5)}</span>
                        <span className="font-mono text-[11px] text-muted-foreground block 2xl:hidden">{truncateMiddle(bottle.user_id, 8, 4)}</span>
                        <CopyButton className='ml-1' value={bottle.user_id} />
                      </div>
                    </TableCell>
                    <TableCell>
                      {(() => {
                        if (bottle.station_id) {
                          return (
                            <div>
                              <div className="text-sm leading-tight">
                                <span className="text-purple-600 font-medium">Estação</span> - {bottle.station_name}
                              </div>
                              <div className="flex items-center gap-0.5 mt-0.5">
                                <span className="font-mono text-[11px] text-muted-foreground hidden 2xl:block">{truncateMiddle(bottle.station_id, 20, 5)}</span>
                                <span className="font-mono text-[11px] text-muted-foreground block 2xl:hidden">{truncateMiddle(bottle.station_id, 8, 4)}</span>
                                <CopyButton className='ml-1' value={bottle.station_id} />
                              </div>
                            </div>
                          );
                        }
                        if (bottle.route_id) {
                          return (
                            <div>
                              <div className="text-sm leading-tight">
                                <span className="text-orange-600 font-medium">Caminhão</span> - {bottle.truck_license_plate ?? bottle.route_id}
                              </div>
                              <div className="flex items-center gap-0.5 mt-0.5">
                                <span className="font-mono text-[11px] text-muted-foreground hidden 2xl:block">{truncateMiddle(bottle.route_id, 20, 5)}</span>
                                <span className="font-mono text-[11px] text-muted-foreground block 2xl:hidden">{truncateMiddle(bottle.route_id, 8, 4)}</span>
                                <CopyButton className='ml-1' value={bottle.route_id} />
                              </div>
                            </div>
                          );
                        }
                        if (bottle.container_id) {
                          return (
                            <div>
                              <div className="text-sm leading-tight">
                                <span className="text-blue-600 font-medium">Container</span> - {bottle.container_name}
                              </div>
                              <div className="flex items-center gap-0.5 mt-0.5">
                                <span className="font-mono text-[11px] text-muted-foreground hidden 2xl:block">{truncateMiddle(bottle.container_id, 20, 5)}</span>
                                <span className="font-mono text-[11px] text-muted-foreground block 2xl:hidden">{truncateMiddle(bottle.container_id, 8, 4)}</span>
                                <CopyButton className='ml-1' value={bottle.container_id} />
                              </div>
                            </div>
                          );
                        }
                        return <span className="text-muted-foreground">-</span>;
                      })()}
                    </TableCell>
                    <TableCell className="text-sm whitespace-nowrap">
                      {Number(bottle.volume_ml).toFixed(1)}ml
                    </TableCell>
                    <TableCell>
                      {(() => {
                        const StageIcon = STAGE_ICONS[bottle.current_stage] || SquareArrowRightEnter;

                        return (
                          <Badge
                            variant="secondary"
                            className={STAGE_COLORS[bottle.current_stage] || ''}
                          >
                            <StageIcon size={12} strokeWidth={3} className="mr-1 mb-[0.25px]" />
                            {t(STAGE_LABELS, bottle.current_stage)}
                          </Badge>
                        );
                      })()}
                    </TableCell>
                    <TableCell>
                      {bottle.utxo_hash ? (
                        <div className="flex items-center gap-0.5">
                          <span className="font-mono text-xs hidden 2xl:block">{truncateMiddle(bottle.utxo_hash, 20, 8)}#{bottle.utxo_index}</span>
                          <span className="font-mono text-xs block 2xl:hidden">{truncateMiddle(bottle.utxo_hash, 15, 6)}#{bottle.utxo_index}</span>
                          <CopyButton className='ml-1' value={`${bottle.utxo_hash}#${bottle.utxo_index}`} />
                        </div>
                      ) : <span className="text-muted-foreground">-</span>}
                    </TableCell>
                    <TableCell className="text-xs text-muted-foreground whitespace-nowrap">
                      {new Date(bottle.inserted_at).toLocaleDateString('pt-BR')}
                    </TableCell>
                  </TableRow>
                );
              })}
              {!loading && bottles.length === 0 && (
                <TableRow>
                  <TableCell colSpan={6} className="text-center text-muted-foreground py-8">
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
