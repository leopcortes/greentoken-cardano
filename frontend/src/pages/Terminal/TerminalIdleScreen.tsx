import { useEffect, useState } from 'react';
import { toast } from 'sonner';
import { Button } from '@/components/ui/button';
import { OwnerLoginDialog } from './OwnerLoginDialog';
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
import { QrCodeMock } from '@/components/ui/qr-code-mock';
import { useAuth } from '@/auth/AuthContext';
import { getRecyclersForTerminal } from '@/services/api';

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

export function TerminalIdleScreen() {
  const { loginRecycler } = useAuth();
  const [open, setOpen] = useState(false);
  const [ownerDialogOpen, setOwnerDialogOpen] = useState(false);
  const [submitting, setSubmitting] = useState(false);
  const [selectedRecyclerId, setSelectedRecyclerId] = useState<string>('');

  const [recyclers, setRecyclers] = useState<Array<{ id: string; name: string; wallet_address: string }>>([]);
  useEffect(() => {
    if (!open) return;
    getRecyclersForTerminal()
      .then(setRecyclers)
      .catch(() => setRecyclers([]));
  }, [open]);

  // Reset state when dialog fecha para que o proximo open comece zerado
  useEffect(() => {
    if (!open) setSelectedRecyclerId('');
  }, [open]);

  const [scanning, setScanning] = useState(false);

  const onSimulateScan = async () => {
    const user = recyclers.find((r) => r.id === selectedRecyclerId);
    if (!user) return;
    setSubmitting(true);
    setScanning(true);
    try {
      await new Promise((r) => setTimeout(r, 1500));
      const logged = await loginRecycler(user.wallet_address);
      toast.success(`Bem-vindo(a), ${logged.name}!`);
      setOpen(false);
    } catch (err) {
      toast.error(humanizeApiError(err, 'Falha ao entrar'), { duration: 5000 });
    } finally {
      setScanning(false);
      setSubmitting(false);
    }
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
        <div className="flex gap-3">
          <span className="gt-chip gt-chip--cdn px-4">Cardano · preprod</span>
          <Button
            type="button"
            onClick={() => {
              setOpen(false);
              setOwnerDialogOpen(true);
            }}
            className=" inline-flex items-center text-sm gap-[6px] px-6 py-[4px] rounded-lg border border-linetext-[12px] font-semibold transition-colors"
          >
            Administração
          </Button>
        </div>
      </header>

      <main className="flex-1 flex items-center justify-center px-6">
        <div className="max-w-xl text-center">
          <div className="text-6xl mb-6">♻️</div>
          <h1
            className="text-4xl font-extrabold tracking-tight text-ink mb-3"
            style={{ letterSpacing: '-0.02em' }}
          >
            Recicle garrafas PET e ganhe <span className="text-gt-700">Greentokens</span>
          </h1>
          <p className="text-ink-3 text-base mb-10 leading-relaxed">
            Identifique-se com a sua greenwallet para iniciar uma sessão.
            Cada garrafa inserida é registrada na blockchain Cardano e gera recompensas.
          </p>
          <Button
            size="lg"
            onClick={() => {
              setOwnerDialogOpen(false);
              setOpen(true);
            }}
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
            <DialogTitle>Entrar com sua Greenwallet</DialogTitle>
          </DialogHeader>

          <div className="flex flex-col items-center gap-2 pt-1">
            <QrCodeMock size={160} scanning={scanning} />
            <p className="text-[12px] text-ink-3 text-center">
              {scanning
                ? 'Lendo QR code...'
                : 'Aponte o app da sua carteira para o QR code acima.'}
            </p>
          </div>

          <div className="relative my-2">
            <div className="absolute inset-0 flex items-center">
              <span className="w-full border-t border-line" />
            </div>
            <div className="relative flex justify-center text-[10px] uppercase tracking-wider">
              <span className="bg-bg-card px-2 text-ink-4">ou (demo)</span>
            </div>
          </div>

          <div className="space-y-3">
            <div>
              <div className="gt-eyebrow mb-1.5">Selecione um reciclador</div>
              <Select
                value={selectedRecyclerId || undefined}
                onValueChange={(v) => setSelectedRecyclerId(v)}
                disabled={submitting || recyclers.length === 0}
              >
                <SelectTrigger>
                  <SelectValue placeholder={recyclers.length === 0 ? 'Carregando...' : 'Selecionar...'} />
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

            <div className="flex justify-end gap-2 pt-2">
              <Button type="button" variant="outline" onClick={() => setOpen(false)} disabled={submitting}>
                Cancelar
              </Button>
              <Button
                type="button"
                onClick={onSimulateScan}
                disabled={submitting || !selectedRecyclerId}
                className="bg-gt-600 hover:bg-gt-700 text-white px-6"
              >
                {scanning ? 'Lendo QR...' : submitting ? 'Entrando...' : 'Entrar'}
              </Button>
            </div>
          </div>
        </DialogContent>
      </Dialog>
      <OwnerLoginDialog open={ownerDialogOpen} onOpenChange={setOwnerDialogOpen} />
    </div>
  );
}
