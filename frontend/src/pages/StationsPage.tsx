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
import { useSortable } from '@/hooks/useSortable';
import {
  getStations, createStation, getStationBottles, shredStation,
  type Station, type Bottle,
} from '@/services/api';
import { STAGE_LABELS, t } from '@/lib/labels';

const STAGE_COLORS: Record<string, string> = {
  atstation: 'bg-purple-100 text-purple-800',
  shredded: 'bg-green-100 text-green-800',
};

export function StationsPage() {
  const [stations, setStations] = useState<Station[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');

  // Create dialog
  const [dialogOpen, setDialogOpen] = useState(false);
  const [submitting, setSubmitting] = useState(false);
  const [formName, setFormName] = useState('');
  const [nameError, setNameError] = useState('');
  const [formLocation, setFormLocation] = useState('');
  const [locationError, setLocationError] = useState('');
  const [formLatitude, setFormLatitude] = useState('');
  const [latitudeError, setLatitudeError] = useState('');
  const [formLongitude, setFormLongitude] = useState('');
  const [longitudeError, setLongitudeError] = useState('');

  // Bottles dialog
  const [selectedStation, setSelectedStation] = useState<Station | null>(null);
  const [stationBottles, setStationBottles] = useState<Bottle[]>([]);
  const [bottlesLoading, setBottlesLoading] = useState(false);
  const [shredding, setShredding] = useState(false);

  const { sorted, sortKey, sortDir, toggleSort } = useSortable<Station>(stations);

  const fetchStations = async () => {
    try {
      setLoading(true);
      const data = await getStations();
      setStations(data);
      setError('');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao carregar estações');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => { fetchStations(); }, []);

  const handleCreate = async (e: React.FormEvent) => {
    e.preventDefault();

    let hasError = false;

    if (!formName.trim()) {
      setNameError('O nome é obrigatório.');
      hasError = true;
    } else {
      setNameError('');
    }

    if (!formLocation.trim()) {
      setLocationError('A localização é obrigatória.');
      hasError = true;
    } else {
      setLocationError('');
    }

    if (!formLatitude.trim()) {
      setLatitudeError('A latitude é obrigatória.');
      hasError = true;
    } else {
      setLatitudeError('');
    }

    if (!formLongitude.trim()) {
      setLongitudeError('A longitude é obrigatória.');
      hasError = true;
    } else {
      setLongitudeError('');
    }

    if (hasError) return;

    setSubmitting(true);

    try {
      await createStation({
        name: formName,
        ...(formLocation ? { location_name: formLocation } : {}),
        ...(formLatitude ? { latitude: Number(formLatitude) } : {}),
        ...(formLongitude ? { longitude: Number(formLongitude) } : {}),
      });
      toast.success(`Estação "${formName}" criada com sucesso.`);
      setDialogOpen(false);
      fetchStations();
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Erro ao criar estação');
    } finally {
      setSubmitting(false);
    }
  };

  const handleViewBottles = async (station: Station) => {
    setSelectedStation(station);
    setBottlesLoading(true);
    try {
      const bottles = await getStationBottles(station.id);
      setStationBottles(bottles);
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Erro ao carregar garrafas');
    } finally {
      setBottlesLoading(false);
    }
  };

  const handleShredAll = async () => {
    if (!selectedStation) return;
    setShredding(true);
    const toastId = toast.loading(`Triturando garrafas na estação "${selectedStation.name}"... Aguardando transações na blockchain.`);
    try {
      const result = await shredStation(selectedStation.id);
      toast.success(result.message, { id: toastId });
      const bottles = await getStationBottles(selectedStation.id);
      setStationBottles(bottles);
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Erro ao triturar garrafas', { id: toastId });
    } finally {
      setShredding(false);
    }
  };

  const formatCoord = (lat: number | null, lng: number | null) => {
    if (lat == null || lng == null) return '-';
    return `${lat.toFixed(5)}, ${lng.toFixed(5)}`;
  };

  const SH = (label: string, key: keyof Station) => (
    <SortableHeader label={label} sortKey={key as string} currentKey={sortKey as string | null} direction={sortDir} onSort={() => toggleSort(key)} />
  );

  const atstationCount = stationBottles.filter(b => b.current_stage === 'atstation').length;
  const shreddedCount = stationBottles.filter(b => b.current_stage === 'shredded').length;

  return (
    <div className="space-y-3">
      <div className="flex items-center justify-between">
        <h2 className="text-lg font-semibold">Estações de Tratamento</h2>
        <div className="flex gap-2">
          <Button variant="outline" size="sm" className="bg-gray-100 text-gray-800" onClick={fetchStations}>Atualizar</Button>
          <Dialog open={dialogOpen} onOpenChange={setDialogOpen}>
            <DialogTrigger asChild>
              <Button size="sm" onClick={() => {
                setFormName('');
                setFormLocation('');
                setFormLatitude('');
                setFormLongitude('');
                setNameError('');
                setLocationError('');
                setLatitudeError('');
                setLongitudeError('');
                setDialogOpen(true);
              }}>+ Nova Estação</Button>
            </DialogTrigger>
            <DialogContent>
              <DialogHeader>
                <DialogTitle>Criar Estação de Tratamento</DialogTitle>
              </DialogHeader>
              <form onSubmit={handleCreate} className="space-y-4">
                <div className="space-y-2">
                  <Label htmlFor="station-name">
                    Nome<span className="text-red-500">*</span>
                  </Label>
                  <Input
                    id="station-name"
                    value={formName}
                    onChange={e => { setFormName(e.target.value); if (nameError) setNameError(''); }}
                    placeholder="Estação de Reciclagem Norte"
                    className={nameError ? 'border-red-500 focus-visible:ring-red-500' : ''}
                  />
                  {nameError && <p className="text-sm font-medium text-red-500">{nameError}</p>}
                </div>

                <div className="space-y-2">
                  <Label htmlFor="station-location">
                    Local<span className="text-red-500">*</span>
                  </Label>
                  <Input 
                    id="station-location" 
                    value={formLocation} 
                    onChange={e => { setFormLocation(e.target.value); if (locationError) setLocationError(''); }}
                    placeholder="Zona Industrial, Brasília" 
                    className={locationError ? 'border-red-500 focus-visible:ring-red-500' : ''}
                  />
                  {locationError && <p className="text-sm font-medium text-red-500">{locationError}</p>}
                </div>

                <div className="grid grid-cols-2 gap-3">
                  <div className="space-y-2">
                    <Label htmlFor="station-lat">
                      Latitude<span className="text-red-500">*</span>
                    </Label>
                    <Input 
                      id="station-lat" 
                      type="number" 
                      step="any" 
                      value={formLatitude} 
                      onChange={e => { setFormLatitude(e.target.value); if (latitudeError) setLatitudeError(''); }}
                      placeholder="-15.7935"
                      className={latitudeError ? 'border-red-500 focus-visible:ring-red-500' : ''}
                    />
                    {latitudeError && <p className="text-sm font-medium text-red-500">{latitudeError}</p>}
                  </div>
                  <div className="space-y-2">
                    <Label htmlFor="station-lng">
                      Longitude<span className="text-red-500">*</span>
                    </Label>
                    <Input 
                      id="station-lng" 
                      type="number" 
                      step="any" 
                      value={formLongitude} 
                      onChange={e => { setFormLongitude(e.target.value); if (longitudeError) setLongitudeError(''); }}
                      placeholder="-47.8828" 
                      className={longitudeError ? 'border-red-500 focus-visible:ring-red-500' : ''}
                    />
                    {longitudeError && <p className="text-sm font-medium text-red-500">{longitudeError}</p>}
                  </div>
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
            {loading ? 'Carregando...' : `${stations.length} estação(ões) encontrada(s)`}
          </CardTitle>
        </CardHeader>
        <CardContent className="px-4 pb-2 pt-0">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>{SH('Nome', 'name')}</TableHead>
                <TableHead>{SH('Local', 'location_name')}</TableHead>
                <TableHead>Coordenadas</TableHead>
                <TableHead>{SH('Garrafas', 'bottle_count')}</TableHead>
                <TableHead>{SH('Criada em', 'created_at')}</TableHead>
                <TableHead>Ações</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {sorted.map(station => (
                <TableRow key={station.id}>
                  <TableCell>
                    <div className="text-sm font-medium leading-tight">{station.name}</div>
                    <div className="flex items-center gap-0.5 mt-0.5">
                      <span className="font-mono text-[11px] text-muted-foreground">{station.id}</span>
                      <CopyButton className="ml-1" value={station.id} />
                    </div>
                  </TableCell>
                  <TableCell className="text-sm">{station.location_name || '-'}</TableCell>
                  <TableCell className="text-xs font-mono text-muted-foreground whitespace-nowrap">
                    {formatCoord(station.latitude, station.longitude)}
                  </TableCell>
                  <TableCell className="text-sm">
                    {station.bottle_count}
                  </TableCell>
                  <TableCell className="text-xs text-muted-foreground whitespace-nowrap">
                    {new Date(station.created_at).toLocaleDateString('pt-BR')}
                  </TableCell>
                  <TableCell>
                    <Button variant="outline" size="sm" className="h-7 text-xs px-2 bg-gray-100" onClick={() => handleViewBottles(station)}>
                      Ver Garrafas
                    </Button>
                  </TableCell>
                </TableRow>
              ))}
              {!loading && stations.length === 0 && (
                <TableRow>
                  <TableCell colSpan={6} className="text-center text-muted-foreground py-8">
                    Nenhuma estação encontrada
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </CardContent>
      </Card>

      {/* Station Bottles Dialog */}
      <Dialog open={!!selectedStation} onOpenChange={() => setSelectedStation(null)}>
        <DialogContent className="max-w-3xl" onOpenAutoFocus={(e) => e.preventDefault()}>
          <DialogHeader>
            <DialogTitle>Garrafas - {selectedStation?.name}</DialogTitle>
          </DialogHeader>
          {selectedStation && (
            <div className="space-y-3">
              <div className="flex gap-4 text-sm">
                <span>Aguardando trituração: <strong>{atstationCount}</strong></span>
                <span>Trituradas: <strong>{shreddedCount}</strong></span>
              </div>
              {bottlesLoading ? (
                <p className="text-sm text-muted-foreground py-4 text-center">Carregando...</p>
              ) : stationBottles.length === 0 ? (
                <p className="text-sm text-muted-foreground py-4 text-center">Nenhuma garrafa nesta estação</p>
              ) : (
                <>
                  <Table>
                    <TableHeader>
                      <TableRow>
                        <TableHead>Garrafa</TableHead>
                        <TableHead>Volume</TableHead>
                        <TableHead>Estágio</TableHead>
                      </TableRow>
                    </TableHeader>
                    <TableBody>
                      {stationBottles.map(bottle => (
                        <TableRow key={bottle.id}>
                          <TableCell>
                            <div className="text-sm font-medium">{bottle.bottle_id_text}</div>
                            <div className="flex items-center gap-0.5 mt-0.5">
                              <span className="font-mono text-[11px] text-muted-foreground">{bottle.id}</span>
                              <CopyButton className="ml-1" value={bottle.id} />
                            </div>
                          </TableCell>
                          <TableCell className="text-sm whitespace-nowrap">
                            {Number(bottle.volume_ml).toFixed(1)}ml
                          </TableCell>
                          <TableCell>
                            <Badge variant="secondary" className={STAGE_COLORS[bottle.current_stage] || ''}>
                              {t(STAGE_LABELS, bottle.current_stage)}
                            </Badge>
                          </TableCell>
                        </TableRow>
                      ))}
                    </TableBody>
                  </Table>

                  {atstationCount > 0 && (
                    <div className="pt-3 border-t">
                      <Button
                        className="w-full"
                        disabled={shredding}
                        onClick={handleShredAll}
                      >
                        {shredding
                          ? 'Triturando...'
                          : `Triturar Todas (${atstationCount} garrafa${atstationCount > 1 ? 's' : ''})`}
                      </Button>
                    </div>
                  )}
                </>
              )}
            </div>
          )}
        </DialogContent>
      </Dialog>
    </div>
  );
}
