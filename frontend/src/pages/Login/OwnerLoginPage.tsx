import { useState, type FormEvent } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { toast } from 'sonner';
import { Toaster } from 'sonner';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { useAuth } from '@/auth/AuthContext';

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
    // body nao e' JSON
  }
  return body || fallback;
}

export function OwnerLoginPage() {
  const { loginOwner } = useAuth();
  const navigate = useNavigate();
  const [password, setPassword] = useState('');
  const [submitting, setSubmitting] = useState(false);

  const onSubmit = async (e: FormEvent) => {
    e.preventDefault();
    if (!password) return;
    setSubmitting(true);
    try {
      await loginOwner(password);
      navigate('/dashboard', { replace: true });
    } catch (err) {
      toast.error(humanizeApiError(err, 'Falha no login'), { duration: 8000 });
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <div className="min-h-screen flex flex-col bg-bg">
      <header className="border-b border-line bg-bg-card px-6 py-4 flex items-center justify-between">
        <Link to="/" className="flex items-center gap-3 hover:opacity-80 transition-opacity">
          <div
            className="w-9 h-9 rounded-[10px] bg-gt-600 text-white flex items-center justify-center font-extrabold text-base tracking-[-0.04em]"
            style={{ boxShadow: '0 4px 12px rgba(22,163,74,0.4)' }}
          >
            ₲
          </div>
          <div>
            <div className="text-md font-bold tracking-[-0.01em] leading-snug">Greentoken</div>
            <div className="text-[12px] text-ink-3 leading-tight">Acesso administrativo</div>
          </div>
        </Link>
      </header>

      <main className="flex-1 flex items-center justify-center px-6">
        <form
          onSubmit={onSubmit}
          className="gt-card w-full max-w-sm p-6 space-y-4"
        >
          <div>
            <h1 className="text-xl font-bold tracking-tight">Login do owner</h1>
            <p className="text-[12px] text-ink-3 mt-1">
              Informe a senha de administrador para acessar o dashboard.
            </p>
          </div>

          <div className="space-y-2">
            <label className="gt-eyebrow block">Senha</label>
            <Input
              type="password"
              autoFocus
              value={password}
              onChange={(e) => setPassword(e.target.value)}
              disabled={submitting}
              placeholder="•••••••"
            />
          </div>

          <Button
            type="submit"
            disabled={submitting || !password}
            className="w-full bg-gt-600 hover:bg-gt-700 text-white"
          >
            {submitting ? 'Verificando...' : 'Entrar'}
          </Button>

          <div className="text-[11px] text-ink-4 text-center pt-2">
            Reciclador?{' '}
            <Link to="/" className="text-gt-700 hover:underline">
              Voltar para o kiosk
            </Link>
          </div>
        </form>
      </main>
      <Toaster position="bottom-right" richColors closeButton />
    </div>
  );
}
