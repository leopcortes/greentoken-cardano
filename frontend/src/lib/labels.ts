/** Tradução visual de estágios (chave = valor do banco/smart contract). */
export const STAGE_LABELS: Record<string, string> = {
  inserted: 'Inserida',
  compacted: 'Compactada',
  collected: 'Coletada',
  atstation: 'Na estação',
  shredded: 'Triturada',
};

/** Tradução visual de cargos (chave = valor do banco). */
export const ROLE_LABELS: Record<string, string> = {
  owner: 'Proprietário',
  recycler: 'Reciclador',
};

/** Tradução visual de status de rotas. */
export const ROUTE_STATUS_LABELS: Record<string, string> = {
  planned: 'Planejada',
  in_progress: 'Em andamento',
  awaiting_delivery: 'Entrega pendente',
  completed: 'Concluída',
};

/** Tradução visual de status de caminhões. */
export const TRUCK_STATUS_LABELS: Record<string, string> = {
  available: 'Disponível',
  on_route: 'Em rota',
  maintenance: 'Manutenção',
};

/** Tradução visual de status de paradas de rota. */
export const STOP_STATUS_LABELS: Record<string, string> = {
  pending: 'Pendente',
  collected: 'Coletado',
};

/** Tradução visual de status de containers. */
export const CONTAINER_STATUS_LABELS: Record<string, string> = {
  active: 'Ativo',
  ready_for_collection: 'Pronto para coleta',
  in_route: 'Em rota de coleta',
  maintenance: 'Manutenção',
};

/** Helper genérico: retorna label traduzido ou o valor original como fallback. */
export function t(map: Record<string, string>, key: string): string {
  return map[key] ?? key;
}
