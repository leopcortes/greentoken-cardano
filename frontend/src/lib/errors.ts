// Erros do backend chegam no formato `"<status>: <body>"` (api.ts), e o body
// frequentemente é `{"error":"..."}`. Extrai a mensagem legível para o toast.
export function humanizeApiError(err: unknown, fallback: string): string {
  const raw = err instanceof Error ? err.message : String(err ?? fallback);
  const match = raw.match(/^\d+:\s*(.+)$/s);
  const body = match ? match[1] : raw;
  let msg = body;
  try {
    const parsed = JSON.parse(body);
    if (parsed && typeof parsed === 'object' && typeof parsed.error === 'string') {
      msg = parsed.error;
    }
  } catch {
    // body não é JSON - usa como está
  }
  // Mapeamentos de erros técnicos do Cardano/infraestrutura
  if (
    msg.includes('ENOENT') ||
    msg.includes('node.socket') ||
    msg.includes('connect ENOENT') ||
    msg.includes('No such file or directory') && msg.includes('socket')
  ) {
    return 'Nó Cardano não está ativo. Aguarde o nó iniciar e tente novamente.';
  }
  if (msg.includes('All inputs are spent')) {
    return 'Transação já incluída na blockchain. Aguarde a confirmação.';
  }
  if (msg.includes('ShelleyTxValidationError') || msg.includes('ConwayApplyTxError')) {
    return 'Erro de validação da transação Cardano. Verifique o estado do nó e tente novamente.';
  }
  return msg || fallback;
}
