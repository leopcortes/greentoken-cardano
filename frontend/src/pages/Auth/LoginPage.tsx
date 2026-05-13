import { useState, type FormEvent } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { toast } from 'sonner';
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
  } catch {}
  return body || fallback;
}

export function LoginPage() {
  const { login } = useAuth();
  const navigate = useNavigate();
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const [submitting, setSubmitting] = useState(false);

  const onSubmit = async (e: FormEvent) => {
    e.preventDefault();
    if (!email || !password) return;
    setSubmitting(true);
    try {
      const u = await login(email, password);
      toast.success(`Bem-vindo(a), ${u.name}!`);
      navigate(u.role === 'owner' ? '/dashboard' : '/', { replace: true });
    } catch (err) {
      toast.error(humanizeApiError(err, 'Falha no login'), { duration: 6000 });
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <div className="min-h-screen flex items-center justify-center bg-bg px-6">
      <div className="w-full max-w-sm">
        <div className="flex items-center gap-3 mb-8">
          <div
            className="w-10 h-10 rounded-[10px] bg-gt-600 text-white flex items-center justify-center font-extrabold text-lg tracking-[-0.04em]"
            style={{ boxShadow: '0 4px 12px rgba(22,163,74,0.4)' }}
          >
            ₲
          </div>
          <div>
            <div className="text-md font-bold tracking-[-0.01em]">Greentoken</div>
            <div className="text-[12px] text-ink-3">Cardano · preprod</div>
          </div>
        </div>

        <h1 className="text-2xl font-extrabold tracking-tight text-ink mb-1">Entrar</h1>
        <p className="text-ink-3 text-sm mb-6">
          Use seu email e senha cadastrados.
        </p>

        <form onSubmit={onSubmit} className="space-y-4">
          <div className="space-y-1.5">
            <label className="gt-eyebrow block" htmlFor="email">Email</label>
            <Input
              id="email"
              type="email"
              autoFocus
              autoComplete="email"
              value={email}
              onChange={(e) => setEmail(e.target.value)}
              disabled={submitting}
              placeholder="email@exemplo.com"
            />
          </div>

          <div className="space-y-1.5">
            <label className="gt-eyebrow block" htmlFor="password">Senha</label>
            <Input
              id="password"
              type="password"
              autoComplete="current-password"
              value={password}
              onChange={(e) => setPassword(e.target.value)}
              disabled={submitting}
              placeholder="••••••••"
            />
          </div>

          <Button
            type="submit"
            disabled={submitting || !email || !password}
            className="w-full bg-gt-600 hover:bg-gt-700 text-white"
          >
            {submitting ? 'Entrando...' : 'Entrar'}
          </Button>
        </form>

        <div className="mt-6 text-center text-sm text-ink-3">
          Ainda não tem uma conta?{' '}
          <Link to="/signup" className="text-gt-700 font-semibold hover:underline">
            Criar conta
          </Link>
        </div>
      </div>
    </div>
  );
}
