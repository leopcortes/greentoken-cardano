import { AlertCircle, X } from 'lucide-react';

interface ErrorAlertProps {
  message: string;
  onDismiss?: () => void;
}

/** Extrai a mensagem legivel de erros da API (ex: '400: {"error":"..."}') */
function parseApiError(raw: string): string {
  // Tenta extrair JSON do corpo da resposta
  const jsonMatch = raw.match(/\d+:\s*(\{.*\})/);
  if (jsonMatch) {
    try {
      const parsed = JSON.parse(jsonMatch[1]);
      if (parsed.error) return parsed.error;
    } catch { /* ignora */ }
  }

  // Remove prefixo de status HTTP se existir
  const statusMatch = raw.match(/^\d{3}:\s*(.*)/);
  if (statusMatch) return statusMatch[1];

  return raw;
}

export function ErrorAlert({ message, onDismiss }: ErrorAlertProps) {
  const friendly = parseApiError(message);

  return (
    <div className="flex items-start gap-3 rounded-lg border border-red-200 bg-red-50 px-4 py-3 text-sm text-red-800">
      <AlertCircle className="h-4 w-4 mt-0.5 shrink-0 text-red-500" />
      <p className="flex-1">{friendly}</p>
      {onDismiss && (
        <button type="button" onClick={onDismiss} className="shrink-0 rounded p-0.5 hover:bg-red-100 transition-colors">
          <X className="h-3.5 w-3.5" />
        </button>
      )}
    </div>
  );
}
