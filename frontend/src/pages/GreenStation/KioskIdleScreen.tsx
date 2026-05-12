import { useEffect, useState, type FormEvent } from 'react';
import { toast } from 'sonner';
import { Link } from 'react-router-dom';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import { useAuth } from '@/auth/AuthContext';
import { getRecyclersForKiosk } from '@/services/api';

function humanizeApiError(err: unknown, fallback: string): string {
  const raw = err instanceof Error ? err.message : String(err ?? fallback);
  const match = raw.match(/^\d+:\s*(.+)$/s);
  const body = match ? match[1] : raw;
  try {
    const parsed = JSON.parse(body);
    if (parsed && typeof parsed === 'object' && typeof parsed.error === 'string') {
      return parsed.error;
    }
  } catch {
    // body nao é JSON
  }
  return body || fallback;
}

export function KioskIdleScreen() {
  const { loginRecycler } = useAuth();
  const [open, setOpen] = useState(false);
  const [address, setAddress] = useState('');
  const [submitting, setSubmitting] = useState(false);

  // Lista publica de recicladores para o "modo demo" (selecionar da lista em
  // vez de digitar o endereco da carteira).
  const [recyclers, setRecyclers] = useState<Array<{ id: string; name: string; wallet_address: string }>>([]);
  useEffect(() => {
    if (!open) return;
    getRecyclersForKiosk().then(setRecyclers).catch(() => setRecyclers([]));
  }, [open]);

  const submit = async (raw: string) => {
    const value = raw.trim();
    if (!value) {
      toast.error('Informe o endereco da sua carteira');
      return;
    }
    setSubmitting(true);
    try {
      const user = await loginRecycler(value);
      toast.success(`Bem-vindo(a), ${user.name}!`);
      setOpen(false);
    } catch (err) {
      toast.error(humanizeApiError(err, 'Falha ao entrar'), { duration: 8000 });
    } finally {
      setSubmitting(false);
    }
  };

  const onFormSubmit = (e: FormEvent) => {
    e.preventDefault();
    void submit(address);
  };

  return (
    <div className="min-h-screen flex flex-col bg-bg">
      <header className="border-b border-line bg-bg-card px-6 py-4 flex items-center justify-between">
        <div className="flex items-center gap-3">
          <div
            className="w-9 h-9 rounded-[10px] bg-gt-600 text-white flex items-center justify-center font-extrabold text-base tracking-[-0.04em]"
            style={{ boxShadow: '0 4px 12px rgba(22,163,74,0.4)' }}
          >
            ₲
          </div>
          <div>
            <div className="text-md font-bold tracking-[-0.01em] leading-snug">
              Greentoken Station
            </div>
            <div className="text-[12px] text-ink-3 leading-tight">
              Container de reciclagem na blockchain Cardano
            </div>
          </div>
        </div>
        <div className="flex gap-2">
          <span className="gt-chip gt-chip--cdn">Cardano · preprod</span>
          <Link
            to="/login/owner"
            className="inline-flex items-center gap-[6px] px-3 py-[6px] rounded-lg border border-line text-ink-2 text-[11px] font-semibold no-underline hover:bg-bg-elev transition-colors"
          >
            Owner
          </Link>
        </div>
      </header>

      <main className="flex-1 flex items-center justify-center px-6">
        <div className="max-w-xl text-center">
          <div className="text-6xl mb-6">♻️</div>
          <h1
            className="text-4xl font-extrabold tracking-tight text-ink mb-3"
            style={{ letterSpacing: '-0.02em' }}
          >
            Recicle suas garrafas e ganhe <span className="text-gt-700">Greentokens</span>
          </h1>
          <p className="text-ink-3 text-base mb-10 leading-relaxed">
            Identifique-se com a sua greenwallet para iniciar uma sessão.
            Cada garrafa inserida é registrada na blockchain Cardano e gera recompensas.
          </p>
          <Button
            size="lg"
            onClick={() => setOpen(true)}
            className="bg-gt-600 hover:bg-gt-700 text-white text-lg font-bold px-8 py-6 h-auto"
            style={{ boxShadow: '0 4px 14px rgba(22,163,74,0.35)' }}
          >
            Toque para começar
          </Button>
          <div className="mt-12 text-[11px] text-ink-4">
            Sem greenwallet?{' '}
            <span className="text-ink-3">
              Procure um responsável para criar a sua.
            </span>
          </div>
        </div>
      </main>

      <Dialog open={open} onOpenChange={setOpen}>
        <DialogContent className="max-w-md">
          <DialogHeader>
            <DialogTitle>Entrar com sua greenwallet</DialogTitle>
          </DialogHeader>
          <p className="text-[12px] text-ink-3 -mt-1 mb-2">
            Informe o endereço Cardano da sua greenwallet. Em produção, esta etapa
            seria substituida por escaneamento de QR code com o app da carteira.
          </p>

          <form onSubmit={onFormSubmit} className="space-y-3">
            <Input
              autoFocus
              placeholder="addr_test1..."
              value={address}
              onChange={(e) => setAddress(e.target.value)}
              disabled={submitting}
              className="mono text-xs"
            />

            {recyclers.length > 0 && (
              <div>
                <div className="gt-eyebrow mb-1.5">Ou selecione um reciclador (demo)</div>
                <Select
                  onValueChange={(v) => {
                    const u = recyclers.find((r) => r.id === v);
                    if (u?.wallet_address) {
                      setAddress(u.wallet_address);
                      void submit(u.wallet_address);
                    }
                  }}
                  disabled={submitting}
                >
                  <SelectTrigger>
                    <SelectValue placeholder="Selecionar..." />
                  </SelectTrigger>
                  <SelectContent>
                    {recyclers.map((r) => (
                      <SelectItem key={r.id} value={r.id}>
                        {r.name}
                      </SelectItem>
                    ))}
                  </SelectContent>
                </Select>
              </div>
            )}

            <div className="flex justify-end gap-2 pt-2">
              <Button type="button" variant="outline" onClick={() => setOpen(false)} disabled={submitting}>
                Cancelar
              </Button>
              <Button
                type="submit"
                disabled={submitting || !address.trim()}
                className="bg-gt-600 hover:bg-gt-700 text-white"
              >
                {submitting ? 'Entrando...' : 'Entrar'}
              </Button>
            </div>
          </form>
        </DialogContent>
      </Dialog>
    </div>
  );
}
