const BASE = '/api';

async function request<T>(path: string, options?: RequestInit): Promise<T> {
  const res = await fetch(`${BASE}${path}`, {
    headers: { 'Content-Type': 'application/json' },
    ...options,
  });
  if (!res.ok) {
    const body = await res.text();
    throw new Error(`${res.status}: ${body}`);
  }
  return res.json();
}

// --- Types (matching real DB schema) ---

export interface User {
  id: string;
  role: 'recycler' | 'owner';
  name: string;
  email: string;
  wallet_address: string | null;
  pubkey_hash: string | null;
  // true se o usuario tem mnemonica custodiada (greenwallet); false para
  // usuarios legados criados antes da migracao 002 com wallet_address manual.
  has_greenwallet: boolean;
  created_at: string;
}

export interface CreatedUser extends User {
  // Mnemonic so vem na resposta de POST /users (greenwallet recem-gerada).
  // Nunca mais sera exposta sem reautenticacao.
  mnemonic: string[];
}

export interface GreenwalletAsset {
  unit: string;
  quantity: string;
}

export interface GreenwalletBalance {
  address: string;
  lovelace: string;
  ada: string;
  greentoken: number;
  assets: GreenwalletAsset[];
}

export interface Bottle {
  id: string;
  user_id: string;
  container_id: string | null;
  container_name: string | null;
  route_id: string | null;
  station_id: string | null;
  station_name: string | null;
  truck_license_plate: string | null;
  bottle_id_text: string;
  bottle_id_hex: string;
  volume_ml: number;
  current_stage: string;
  utxo_hash: string | null;
  utxo_index: number | null;
  inserted_at: string;
  compacted_at: string | null;
  collected_at: string | null;
  atstation_at: string | null;
  shredded_at: string | null;
}

export interface Container {
  id: string;
  owner_id: string;
  name: string;
  location_name: string | null;
  latitude: number | null;
  longitude: number | null;
  capacity_liters: number;
  current_volume_liters: number;
  status: string;
  last_updated: string;
}

export interface BlockchainTx {
  id: string;
  bottle_id: string;
  stage: string;
  tx_hash: string | null;
  status: 'pending' | 'confirmed' | 'failed';
  datum_json: string | null;
  redeemer_json: string | null;
  submitted_at: string;
  confirmed_at: string | null;
}

export interface Reward {
  id: string;
  user_id: string;
  bottle_id: string;
  bottle_name?: string;
  tx_id: string | null;
  stage: string;
  greentoken_amount: number;
  tx_hash: string | null;
  sent_at: string;
}

export interface Truck {
  id: string;
  license_plate: string;
  status: string;
  last_updated: string;
}

export interface Route {
  id: string;
  truck_id: string;
  station_id: string | null;
  status: string;
  created_at: string;
  completed_at: string | null;
  truck_license_plate: string;
  stop_count: number;
  station_name: string | null;
  container_names: string | null;
}

export interface Station {
  id: string;
  name: string;
  location_name: string | null;
  latitude: number | null;
  longitude: number | null;
  created_at: string;
  bottle_count: number;
}

export interface RouteStop {
  id: string;
  route_id: string;
  container_id: string;
  stop_order: number;
  status: string;
  collected_at: string | null;
  container_name: string;
}

// --- Users ---

export const getUsers = (role?: string) =>
  request<User[]>(`/users${role ? `?role=${role}` : ''}`);

export const getUser = (id: string) =>
  request<User>(`/users/${id}`);

export const createUser = (data: {
  role: string; name: string; email: string;
}) =>
  request<CreatedUser>('/users', { method: 'POST', body: JSON.stringify(data) });

export const getUserRewards = (id: string) =>
  request<{ rewards: Reward[]; total_greentoken: number }>(`/users/${id}/rewards`);

export const getGreenwallet = (id: string) =>
  request<{ address: string; pubkey_hash: string }>(`/users/${id}/greenwallet`);

export const getGreenwalletBalance = (id: string) =>
  request<GreenwalletBalance>(`/users/${id}/greenwallet/balance`);

// --- Bottles ---

export const getBottles = (params?: { user_id?: string; stage?: string; container_id?: string; container_name?: string; route_id?: string; station_id?: string; station_name?: string; }) => {
  const query = new URLSearchParams();
  if (params?.user_id) query.set('user_id', params.user_id);
  if (params?.stage) query.set('stage', params.stage);
  if (params?.container_id) query.set('container_id', params.container_id);
  if (params?.container_name) query.set('container_name', params.container_name);
  if (params?.route_id) query.set('route_id', params.route_id);
  if (params?.station_id) query.set('station_id', params.station_id);
  if (params?.station_name) query.set('station_name', params.station_name);
  const qs = query.toString();
  return request<Bottle[]>(`/bottles${qs ? `?${qs}` : ''}`);
};

export const getBottle = (id: string) =>
  request<{ bottle: Bottle; txs: BlockchainTx[]; rewards: Reward[] }>(`/bottles/${id}`);

export const createBottle = (data: { user_id: string; container_id: string; volume_ml: number }) =>
  request<{ bottle: Bottle; tx_hash: string; message: string }>(
    '/bottles', { method: 'POST', body: JSON.stringify(data) }
  );

export const getNextBottleNumber = () =>
  request<{ next_number: number; next_name: string }>('/bottles/next-number');

// --- Container actions ---

// --- Containers ---

export const getContainers = (params?: { status?: string; owner_id?: string }) => {
  const query = new URLSearchParams();
  if (params?.status) query.set('status', params.status);
  if (params?.owner_id) query.set('owner_id', params.owner_id);
  const qs = query.toString();
  return request<Container[]>(`/containers${qs ? `?${qs}` : ''}`);
};

export const createContainer = (data: {
  owner_id: string; name: string; location_name?: string;
  latitude?: number; longitude?: number; capacity_liters: number;
}) =>
  request<Container>('/containers', { method: 'POST', body: JSON.stringify(data) });

// --- Trucks ---

export const getTrucks = () =>
  request<Truck[]>('/trucks');

export const createTruck = (data: { license_plate: string }) =>
  request<Truck>('/trucks', { method: 'POST', body: JSON.stringify(data) });

// --- Routes ---

export const getRoutes = () =>
  request<Route[]>('/routes');

export const getRoute = (id: string) =>
  request<Route & { stops: RouteStop[] }>(`/routes/${id}`);

export const createRoute = (data: { truck_id: string; station_id?: string; container_ids: string[] }) =>
  request<Route>('/routes', { method: 'POST', body: JSON.stringify(data) });

export const collectStop = (stopId: string) =>
  request<{ message: string; collected: number }>(`/routes/stops/${stopId}/collect`, { method: 'POST' });

export const deliverRoute = (routeId: string, stationId: string) =>
  request<{ message: string; delivered: number }>(
    `/routes/${routeId}/deliver`, { method: 'POST', body: JSON.stringify({ station_id: stationId }) }
  );

// --- Stations ---

export const getStations = () =>
  request<Station[]>('/stations');

export const getStation = (id: string) =>
  request<Station>(`/stations/${id}`);

export const createStation = (data: {
  name: string; location_name?: string; latitude?: number; longitude?: number;
}) =>
  request<Station>('/stations', { method: 'POST', body: JSON.stringify(data) });

export const getStationBottles = (stationId: string) =>
  request<Bottle[]>(`/stations/${stationId}/bottles`);

export const shredStation = (stationId: string) =>
  request<{ message: string; stationId: string; shredded: number }>(
    `/stations/${stationId}/shred`, { method: 'POST' }
  );
