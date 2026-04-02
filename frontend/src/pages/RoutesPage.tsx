import { useEffect, useState } from 'react';
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
import { useSortable } from '@/hooks/useSortable';
import {
  getRoutes, createRoute, getRoute, collectStop,
  getTrucks, createTruck,
  getContainers,
  type Route, type RouteStop, type Truck, type Container,
} from '@/services/api';

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
  const [submitting, setSubmitting] = useState(false);

  const [truckDialogOpen, setTruckDialogOpen] = useState(false);
  const [formPlate, setFormPlate] = useState('');

  const [detailRoute, setDetailRoute] = useState<(Route & { stops: RouteStop[] }) | null>(null);

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
      const [trucksData, containersData] = await Promise.all([
        getTrucks(),
        getContainers({ status: 'full' }),
      ]);
      setAvailableTrucks(trucksData.filter(t => t.status === 'available'));
      setFullContainers(containersData);
      setSelectedTruck('');
      setSelectedContainers([]);
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
    if (!selectedTruck || selectedContainers.length === 0) return;
    setSubmitting(true);
    try {
      await createRoute({ truck_id: selectedTruck, container_ids: selectedContainers });
      setRouteDialogOpen(false);
      fetchData();
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao criar rota');
    } finally {
      setSubmitting(false);
    }
  };

  const handleCreateTruck = async (e: React.FormEvent) => {
    e.preventDefault();
    setSubmitting(true);
    try {
      await createTruck({ license_plate: formPlate });
      setTruckDialogOpen(false);
      setFormPlate('');
      fetchData();
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao criar caminhão');
    } finally {
      setSubmitting(false);
    }
  };

  const handleViewRoute = async (routeId: string) => {
    try {
      const data = await getRoute(routeId);
      setDetailRoute(data);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao carregar rota');
    }
  };

  const handleCollectStop = async (stopId: string) => {
    try {
      await collectStop(stopId);
      if (detailRoute) {
        const updated = await getRoute(detailRoute.id);
        setDetailRoute(updated);
      }
      fetchData();
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao coletar parada');
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
                  <Label htmlFor="truck-plate">Placa</Label>
                  <Input id="truck-plate" value={formPlate} onChange={e => setFormPlate(e.target.value)} required placeholder="GRN-0002" />
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
                        {truck.status}
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

      <Separator />

      {/* Routes */}
      <div className="space-y-3">
        <div className="flex items-center justify-between">
          <h2 className="text-lg font-semibold">Rotas de Coleta</h2>
          <div className="flex gap-2">
            <Button variant="outline" size="sm" onClick={fetchData}>Atualizar</Button>
            <Button size="sm" onClick={openCreateRoute}>+ Nova Rota</Button>
          </div>
        </div>

        <Card>
          <CardHeader className="py-3 px-4">
            <CardTitle className="text-sm font-medium text-muted-foreground">
              {loading ? 'Carregando...' : `${routes.length} rota(s) encontrada(s)`}
            </CardTitle>
          </CardHeader>
          <CardContent className="px-4 pb-4 pt-0">
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
                        <span className="font-mono text-xs">{route.id}...</span>
                        <CopyButton className='ml-1' value={route.id} />
                      </div>
                    </TableCell>
                    <TableCell className="font-mono text-sm">{route.truck_license_plate}</TableCell>
                    <TableCell className="text-sm">{route.stop_count} parada(s)</TableCell>
                    <TableCell>
                      <Badge variant="secondary" className={ROUTE_STATUS_COLORS[route.status] || ''}>
                        {route.status}
                      </Badge>
                    </TableCell>
                    <TableCell className="text-xs text-muted-foreground whitespace-nowrap">
                      {new Date(route.created_at).toLocaleDateString('pt-BR')}
                    </TableCell>
                    <TableCell>
                      <Button variant="outline" size="sm" className="h-7 text-xs px-2" onClick={() => handleViewRoute(route.id)}>
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
              <Label>Caminhão</Label>
              {availableTrucks.length > 0 ? (
                <select
                  value={selectedTruck}
                  onChange={e => setSelectedTruck(e.target.value)}
                  className="flex h-10 w-full rounded-md border border-input bg-background px-3 py-2 text-sm"
                  required
                >
                  <option value="">Selecione um caminhão...</option>
                  {availableTrucks.map(t => (
                    <option key={t.id} value={t.id}>{t.license_plate}</option>
                  ))}
                </select>
              ) : (
                <p className="text-sm text-muted-foreground">Nenhum caminhão disponível</p>
              )}
            </div>
            <div className="space-y-2">
              <Label>Containers cheios ({selectedContainers.length} selecionado(s))</Label>
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
            <Button
              type="submit"
              disabled={submitting || !selectedTruck || selectedContainers.length === 0}
              className="w-full"
            >
              {submitting ? 'Criando...' : 'Criar Rota'}
            </Button>
          </form>
        </DialogContent>
      </Dialog>

      {/* Route Detail Dialog */}
      <Dialog open={!!detailRoute} onOpenChange={() => setDetailRoute(null)}>
        <DialogContent className="max-w-lg">
          <DialogHeader>
            <DialogTitle>Rota - {detailRoute?.id}...</DialogTitle>
          </DialogHeader>
          {detailRoute && (
            <div className="space-y-3">
              <div className="flex gap-4 text-sm items-center">
                <span>Caminhão: <strong>{detailRoute.truck_license_plate}</strong></span>
                <Badge variant="secondary" className={ROUTE_STATUS_COLORS[detailRoute.status] || ''}>
                  {detailRoute.status}
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
                          {stop.status}
                        </Badge>
                      </TableCell>
                      <TableCell>
                        {stop.status === 'pending' ? (
                          <Button variant="outline" size="sm" className="h-7 text-xs px-2" onClick={() => handleCollectStop(stop.id)}>
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
            </div>
          )}
        </DialogContent>
      </Dialog>
    </div>
  );
}
