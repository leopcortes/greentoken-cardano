import { useEffect, useState } from 'react';
import { toast } from 'sonner';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Dialog, DialogContent, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Separator } from '@/components/ui/separator';
import { CopyButton } from '@/components/ui/copy-button';
import { ErrorAlert } from '@/components/ui/error-alert';
import { SortableHeader } from '@/components/ui/sortable-header';
import { Select, SelectContent, SelectGroup, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select"
import { useSortable } from '@/hooks/useSortable';
import {
  getRoutes, createRoute, getRoute, collectStop, deliverRoute,
  getTrucks, createTruck,
  getContainers, getBottles,
  getStations,
  type Route, type RouteStop, type Truck, type Container, type Station,
} from '@/services/api';
import { ROUTE_STATUS_LABELS, TRUCK_STATUS_LABELS, STOP_STATUS_LABELS, t } from '@/lib/labels';

const ROUTE_STATUS_COLORS: Record<string, string> = {
  planned: 'bg-blue-100 text-blue-800',
  in_progress: 'bg-yellow-100 text-yellow-800',
  completed: 'bg-green-100 text-green-800',
};

const TRUCK_STATUS_COLORS: Record<string, string> = {
  available: 'bg-green-100 text-green-800',
  on_route: 'bg-yellow-100 text-yellow-800',
  maintenance: 'bg-gray-100 text-gray-800',
};

export function RoutesPage() {
  const [routes, setRoutes] = useState<Route[]>([]);
  const [trucks, setTrucks] = useState<Truck[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');

  const [routeDialogOpen, setRouteDialogOpen] = useState(false);
  const [availableTrucks, setAvailableTrucks] = useState<Truck[]>([]);
  const [fullContainers, setFullContainers] = useState<Container[]>([]);
  const [selectedTruck, setSelectedTruck] = useState('');
  const [selectedContainers, setSelectedContainers] = useState<string[]>([]);
  const [stationOptions, setStationOptions] = useState<Station[]>([]);
  const [selectedStation, setSelectedStation] = useState('');
  const [submitting, setSubmitting] = useState(false);
  const [delivering, setDelivering] = useState(false);

  const [truckDialogOpen, setTruckDialogOpen] = useState(false);
  const [formPlate, setFormPlate] = useState('');
  const [plateError, setPlateError] = useState('');

  const [detailRoute, setDetailRoute] = useState<(Route & { stops: RouteStop[] }) | null>(null);
  const [routeHasCollected, setRouteHasCollected] = useState(false);

  const truckSort = useSortable<Truck>(trucks);
  const routeSort = useSortable<Route>(routes);

  const fetchData = async () => {
    try {
      setLoading(true);
      const [routesData, trucksData] = await Promise.all([getRoutes(), getTrucks()]);
      setRoutes(routesData);
      setTrucks(trucksData);
      setError('');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao carregar dados');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => { fetchData(); }, []);

  const openCreateRoute = async () => {
    try {
      const [trucksData, containersData, stationsData] = await Promise.all([
        getTrucks(),
        getContainers({ status: 'full' }),
        getStations(),
      ]);
      setAvailableTrucks(trucksData.filter(t => t.status === 'available'));
      setFullContainers(containersData);
      setStationOptions(stationsData);
      setSelectedTruck('');
      setSelectedContainers([]);
      setSelectedStation('');
      setRouteDialogOpen(true);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao carregar dados para rota');
    }
  };

  const toggleContainer = (id: string) => {
    setSelectedContainers(prev =>
      prev.includes(id) ? prev.filter(c => c !== id) : [...prev, id]
    );
  };

  const handleCreateRoute = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!selectedTruck || selectedContainers.length === 0 || !selectedStation) return;
    setSubmitting(true);
    try {
      await createRoute({
        truck_id: selectedTruck,
        container_ids: selectedContainers,
        station_id: selectedStation,
      });
      toast.success('Rota de coleta criada com sucesso.');
      setRouteDialogOpen(false);
      fetchData();
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Erro ao criar rota');
    } finally {
      setSubmitting(false);
    }
  };

  const handleCreateTruck = async (e: React.FormEvent) => {
  e.preventDefault();

  if (!formPlate.trim()) {
    setPlateError('A placa é obrigatória.');
    return;
  }

  setPlateError('');
  setSubmitting(true);

  try {
    await createTruck({ license_plate: formPlate });
    toast.success(`Caminhão "${formPlate}" cadastrado com sucesso.`);
    setTruckDialogOpen(false);
    setFormPlate('');
    setPlateError('');
    fetchData();
  } catch (err) {
    toast.error(err instanceof Error ? err.message : 'Erro ao criar caminhão');
  } finally {
    setSubmitting(false);
  }
};

  const handleViewRoute = async (routeId: string) => {
    try {
      const [data, bottles] = await Promise.all([
        getRoute(routeId),
        getBottles({ route_id: routeId }),
      ]);
      setDetailRoute(data);
      setRouteHasCollected(bottles.some(b => b.current_stage === 'collected'));
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao carregar rota');
    }
  };

  const handleCollectStop = async (stopId: string) => {
    const toastId = toast.loading('Coletando parada... Movendo garrafas para o estágio coletadas. Aguardando transações na blockchain.');
    try {
      const result = await collectStop(stopId);
      toast.success(result.message, { id: toastId });
      if (detailRoute) {
        const [updated, bottles] = await Promise.all([
          getRoute(detailRoute.id),
          getBottles({ route_id: detailRoute.id }),
        ]);
        setDetailRoute(updated);
        setRouteHasCollected(bottles.some(b => b.current_stage === 'collected'));
      }
      fetchData();
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Erro ao coletar parada', { id: toastId });
    }
  };

  const handleDeliver = async () => {
    if (!detailRoute) return;

    // Precisa de uma estação: usa a da rota ou pede seleção
    const stationId = detailRoute.station_id;
    if (!stationId) {
      toast.error('Esta rota não tem estação de destino definida. Edite a rota ou selecione uma estação ao criar.');
      return;
    }

    setDelivering(true);
    const toastId = toast.loading('Entregando garrafas na estação... Aguardando transações na blockchain.');
    try {
      const result = await deliverRoute(detailRoute.id, stationId);
      toast.success(result.message, { id: toastId });
      const [updated, bottles] = await Promise.all([
        getRoute(detailRoute.id),
        getBottles({ route_id: detailRoute.id }),
      ]);
      setDetailRoute(updated);
      setRouteHasCollected(bottles.some(b => b.current_stage === 'collected'));
      fetchData();
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Erro ao entregar garrafas', { id: toastId });
    } finally {
      setDelivering(false);
    }
  };

  const TSH = (label: string, key: keyof Truck) => (
    <SortableHeader label={label} sortKey={key as string} currentKey={truckSort.sortKey as string | null} direction={truckSort.sortDir} onSort={() => truckSort.toggleSort(key)} />
  );

  const RSH = (label: string, key: keyof Route) => (
    <SortableHeader label={label} sortKey={key as string} currentKey={routeSort.sortKey as string | null} direction={routeSort.sortDir} onSort={() => routeSort.toggleSort(key)} />
  );

  return (
    <div className="space-y-5">
      {error && <ErrorAlert message={error} onDismiss={() => setError('')} />}

      {/* Routes */}
      <div className="space-y-3">
        <div className="flex items-center justify-between">
          <h2 className="text-lg font-semibold">Rotas de Coleta</h2>
          <div className="flex gap-2">
            <Button variant="outline" size="sm" className='bg-gray-100 text-gray-800' onClick={fetchData}>Atualizar</Button>
            <Button size="sm" onClick={openCreateRoute}>+ Nova Rota</Button>
          </div>
        </div>

        <Card>
          <CardHeader className="py-3 px-4">
            <CardTitle className="text-sm font-medium text-muted-foreground">
              {loading ? 'Carregando...' : `${routes.length} rota(s) encontrada(s)`}
            </CardTitle>
          </CardHeader>
          <CardContent className="px-4 pb-2 pt-0">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>ID</TableHead>
                  <TableHead>{RSH('Caminhão', 'truck_license_plate')}</TableHead>
                  <TableHead>{RSH('Paradas', 'stop_count')}</TableHead>
                  <TableHead>{RSH('Status', 'status')}</TableHead>
                  <TableHead>{RSH('Criada em', 'created_at')}</TableHead>
                  <TableHead>Ações</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {routeSort.sorted.map(route => (
                  <TableRow key={route.id}>
                    <TableCell>
                      <div className="flex items-center gap-0.5">
                        <span className="font-mono text-xs">{route.id}</span>
                        <CopyButton className='ml-1' value={route.id} />
                      </div>
                    </TableCell>
                    <TableCell className="font-mono text-sm">{route.truck_license_plate}</TableCell>
                    <TableCell className="text-sm">{route.stop_count == 1 ? `${route.stop_count} parada` : `${route.stop_count} paradas`}</TableCell>
                    <TableCell>
                      <Badge variant="secondary" className={ROUTE_STATUS_COLORS[route.status] || ''}>
                        {t(ROUTE_STATUS_LABELS, route.status)}
                      </Badge>
                    </TableCell>
                    <TableCell className="text-xs text-muted-foreground whitespace-nowrap">
                      {new Date(route.created_at).toLocaleDateString('pt-BR')}
                    </TableCell>
                    <TableCell>
                      <Button variant="outline" size="sm" className="h-7 text-xs px-2 bg-gray-100" onClick={() => handleViewRoute(route.id)}>
                        Detalhes
                      </Button>
                    </TableCell>
                  </TableRow>
                ))}
                {!loading && routes.length === 0 && (
                  <TableRow>
                    <TableCell colSpan={6} className="text-center text-muted-foreground py-8">
                      Nenhuma rota encontrada
                    </TableCell>
                  </TableRow>
                )}
              </TableBody>
            </Table>
          </CardContent>
        </Card>
      </div>

      {/* Create Route Dialog */}
      <Dialog open={routeDialogOpen} onOpenChange={setRouteDialogOpen}>
        <DialogContent className="max-w-lg">
          <DialogHeader>
            <DialogTitle>Criar Rota de Coleta</DialogTitle>
          </DialogHeader>
          <form onSubmit={handleCreateRoute} className="space-y-4">
            <div className="space-y-2">
              <Label>
                Caminhão<span className="text-red-500">*</span>
              </Label>
              {availableTrucks.length > 0 ? (
                <Select value={selectedTruck} onValueChange={setSelectedTruck}>
                  <SelectTrigger id="route-truck" className="w-full">
                    <SelectValue placeholder="Selecione um caminhão..." />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectGroup>
                      {availableTrucks.map(t => (
                        <SelectItem key={t.id} value={t.id}>
                          {t.license_plate}
                        </SelectItem>
                      ))}
                    </SelectGroup>
                  </SelectContent>
                </Select>
              ) : (
                <p className="text-sm text-muted-foreground">Nenhum caminhão disponível</p>
              )}
            </div>
            <div className="space-y-2">
              <Label>Containers compactados<span className="text-red-500">*</span> ({selectedContainers.length} selecionado(s))</Label>
              {fullContainers.length > 0 ? (
                <div className="space-y-1 max-h-48 overflow-y-auto border rounded-md p-2">
                  {fullContainers.map(c => (
                    <label key={c.id} className="flex items-center gap-2 text-sm cursor-pointer hover:bg-muted rounded px-2 py-1">
                      <input
                        type="checkbox"
                        checked={selectedContainers.includes(c.id)}
                        onChange={() => toggleContainer(c.id)}
                        className="rounded"
                      />
                      <span className="font-medium">{c.name}</span>
                      <span className="text-muted-foreground">
                        - {c.current_volume_liters}/{c.capacity_liters}L
                      </span>
                    </label>
                  ))}
                </div>
              ) : (
                <p className="text-sm text-muted-foreground">Nenhum container cheio no momento</p>
              )}
            </div>
            <div className="space-y-2">
              <Label>Estação de destino<span className="text-red-500">*</span></Label>
              {stationOptions.length > 0 ? (
                <Select value={selectedStation} onValueChange={setSelectedStation}>
                  <SelectTrigger className="w-full">
                    <SelectValue placeholder="Selecione uma estação..." />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectGroup>
                      {stationOptions.map(s => (
                        <SelectItem key={s.id} value={s.id}>
                          {s.name}{s.location_name ? ` - ${s.location_name}` : ''}
                        </SelectItem>
                      ))}
                    </SelectGroup>
                  </SelectContent>
                </Select>
              ) : (
                <p className="text-sm text-muted-foreground">Nenhuma estação cadastrada</p>
              )}
            </div>
            <Button
              type="submit"
              disabled={submitting || !selectedTruck || selectedContainers.length === 0 || !selectedStation}
              className="w-full"
            >
              {submitting ? 'Criando...' : 'Criar Rota'}
            </Button>
          </form>
        </DialogContent>
      </Dialog>

      {/* Route Detail Dialog */}
      <Dialog open={!!detailRoute} onOpenChange={() => setDetailRoute(null)}>
        <DialogContent className="max-w-3xl">
          <DialogHeader>
            <DialogTitle>Rota - {detailRoute?.id}</DialogTitle>
          </DialogHeader>
          {detailRoute && (
            <div className="space-y-3">
              <div className="flex gap-4 text-sm items-center">
                <span>Caminhão: <strong>{detailRoute.truck_license_plate}</strong></span>
                <Badge variant="secondary" className={ROUTE_STATUS_COLORS[detailRoute.status] || ''}>
                  {t(ROUTE_STATUS_LABELS, detailRoute.status)}
                </Badge>
              </div>
              <Table>
                <TableHeader>
                  <TableRow>
                    <TableHead>#</TableHead>
                    <TableHead>Container</TableHead>
                    <TableHead>Status</TableHead>
                    <TableHead>Ação</TableHead>
                  </TableRow>
                </TableHeader>
                <TableBody>
                  {detailRoute.stops?.map(stop => (
                    <TableRow key={stop.id}>
                      <TableCell className="text-sm">{stop.stop_order}</TableCell>
                      <TableCell className="font-medium text-sm">{stop.container_name}</TableCell>
                      <TableCell>
                        <Badge variant="secondary" className={
                          stop.status === 'collected' ? 'bg-green-100 text-green-800' : 'bg-yellow-100 text-yellow-800'
                        }>
                          {t(STOP_STATUS_LABELS, stop.status)}
                        </Badge>
                      </TableCell>
                      <TableCell>
                        {stop.status === 'pending' ? (
                          <Button variant="outline" size="sm" className="h-7 text-xs px-2 bg-gray-100" onClick={() => handleCollectStop(stop.id)}>
                            Coletar
                          </Button>
                        ) : (
                          <span className="text-xs text-muted-foreground">
                            {new Date(stop.collected_at!).toLocaleDateString('pt-BR')}
                          </span>
                        )}
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>

              {/* Botão de entregar na estação: aparece quando TODAS as paradas foram coletadas e estação definida */}
              {(() => {
                const allStopsCollected = detailRoute.stops?.length > 0 && detailRoute.stops.every(s => s.status === 'collected');
                const hasAnyPending = detailRoute.stops?.some(s => s.status === 'pending');

                if (allStopsCollected && routeHasCollected && detailRoute.station_id) {
                  return (
                    <div className="pt-3 border-t">
                      <Button
                        className="w-full"
                        disabled={delivering}
                        onClick={handleDeliver}
                      >
                        {delivering ? 'Entregando...' : 'Entregar Garrafas na Estação'}
                      </Button>
                    </div>
                  );
                }

                if (routeHasCollected && hasAnyPending && detailRoute.station_id) {
                  return (
                    <p className="text-sm text-amber-600 pt-3 border-t text-center">
                      Todas as paradas devem ser coletadas antes de entregar na estação.
                    </p>
                  );
                }

                if (detailRoute.status === 'completed' && !routeHasCollected && detailRoute.station_id) {
                  return (
                    <p className="text-sm text-muted-foreground pt-3 border-t text-center">
                      Garrafas já entregues na estação.
                    </p>
                  );
                }

                if (detailRoute.status === 'completed' && !detailRoute.station_id) {
                  return (
                    <p className="text-sm text-muted-foreground pt-3 border-t">
                      Rota concluída, mas sem estação de destino definida.
                    </p>
                  );
                }

                return null;
              })()}
            </div>
          )}
        </DialogContent>
      </Dialog>

      <Separator />

      {/* Trucks */}
      <div className="space-y-3">
        <div className="flex items-center justify-between">
          <h2 className="text-lg font-semibold">Caminhões</h2>
          <Dialog open={truckDialogOpen} onOpenChange={setTruckDialogOpen}>
            <DialogTrigger asChild>
              <Button size="sm">+ Novo Caminhão</Button>
            </DialogTrigger>
            <DialogContent>
              <DialogHeader>
                <DialogTitle>Cadastrar Caminhão</DialogTitle>
              </DialogHeader>
              <form onSubmit={handleCreateTruck} className="space-y-4">
                <div className="space-y-2">
                <Label htmlFor="truck-plate">
                  Placa<span className="text-red-500">*</span>
                </Label>

                <Input
                  id="truck-plate"
                  value={formPlate}
                  onChange={e => {
                    setFormPlate(e.target.value);
                    if (plateError) setPlateError('');
                  }}
                  placeholder="GRN-0002"
                  className={plateError ? 'border-red-500 focus-visible:ring-red-500' : ''}
                />

                {plateError && (
                  <p className="text-sm font-medium text-red-500">{plateError}</p>
                )}
              </div>
                <Button type="submit" disabled={submitting} className="w-full">
                  {submitting ? 'Criando...' : 'Criar'}
                </Button>
              </form>
            </DialogContent>
          </Dialog>
        </div>

        <Card>
          <CardContent className="px-4 py-4">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>{TSH('Placa', 'license_plate')}</TableHead>
                  <TableHead>{TSH('Status', 'status')}</TableHead>
                  <TableHead>{TSH('Atualizado em', 'last_updated')}</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {truckSort.sorted.map(truck => (
                  <TableRow key={truck.id}>
                    <TableCell className="font-mono font-medium text-sm">{truck.license_plate}</TableCell>
                    <TableCell>
                      <Badge variant="secondary" className={TRUCK_STATUS_COLORS[truck.status] || ''}>
                        {t(TRUCK_STATUS_LABELS, truck.status)}
                      </Badge>
                    </TableCell>
                    <TableCell className="text-xs text-muted-foreground whitespace-nowrap">
                      {new Date(truck.last_updated).toLocaleDateString('pt-BR')}
                    </TableCell>
                  </TableRow>
                ))}
                {!loading && trucks.length === 0 && (
                  <TableRow>
                    <TableCell colSpan={3} className="text-center text-muted-foreground py-6">
                      Nenhum caminhão cadastrado
                    </TableCell>
                  </TableRow>
                )}
              </TableBody>
            </Table>
          </CardContent>
        </Card>
      </div>
    </div>
  );
}
