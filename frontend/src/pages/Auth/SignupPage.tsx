import { useState, type FormEvent } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { toast } from 'sonner';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { useAuth } from '@/auth/AuthContext';
import type { SignupMode, User } from '@/services/api';
import { humanizeApiError } from '@/lib/errors';

interface MnemonicGateProps {
  words: string[];
  onConfirm: () => void;
}

interface PendingSession {
  mnemonic: string[];
  token: string;
  user: User;
}

// Tela bloqueante apos signup com greenwallet nova: forca o usuário a anotar
// as 24 palavras antes de seguir para o app. Mnemonica NUNCA mais sera exposta
// sem reautenticacao.
function MnemonicGate({ words, onConfirm }: MnemonicGateProps) {
  const [acknowledged, setAcknowledged] = useState(false);

  return (
    <div className="min-h-screen flex items-center justify-center bg-bg px-6 py-12">
      <div className="w-full max-w-2xl">
        <h1 className="text-2xl font-extrabold tracking-tight text-ink mb-2">
          Salve suas 24 palavras
        </h1>
        <p className="text-ink-3 text-sm mb-6">
          Esta é a sua greenwallet. Anote as palavras na ordem exata em local seguro -
          sem elas você não consegue recuperar sua conta.
          <strong className="text-ink"> Não compartilhamos isto outra vez.</strong>
        </p>

        <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 gap-2 mb-6">
          {words.map((w, i) => (
            <div
              key={i}
              className="border border-line rounded-md px-3 py-2 bg-bg-card text-sm"
            >
              <span className="text-ink-4 text-[11px] mr-1.5">{i + 1}.</span>
              <span className="font-mono">{w}</span>
            </div>
          ))}
        </div>

        <div className="flex items-start gap-2 mb-6">
          <input
            id="ack"
            type="checkbox"
            checked={acknowledged}
            onChange={(e) => setAcknowledged(e.target.checked)}
            className="mt-1"
          />
          <label htmlFor="ack" className="text-sm text-ink-3">
            Anotei as 24 palavras em local seguro e entendo que perdê-las significa
            perder o acesso à wallet.
          </label>
        </div>

        <Button
          type="button"
          disabled={!acknowledged}
          onClick={onConfirm}
          className="bg-gt-600 hover:bg-gt-700 text-white"
        >
          Continuar
        </Button>
      </div>
    </div>
  );
}

export function SignupPage() {
  const { signup, loginWithToken } = useAuth();
  const navigate = useNavigate();

  const [name, setName] = useState('');
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const [confirm, setConfirm] = useState('');
  const [mode, setMode] = useState<SignupMode>('new_greenwallet');
  const [mnemonicInput, setMnemonicInput] = useState('');
  const [walletAddress, setWalletAddress] = useState('');
  const [submitting, setSubmitting] = useState(false);
  // Guarda mnemonic + credenciais enquanto o usuário ainda não confirmou as 24
  // palavras. Só fazemos login de fato no onConfirm.
  const [pendingSession, setPendingSession] = useState<PendingSession | null>(null);

  const onSubmit = async (e: FormEvent) => {
    e.preventDefault();
    if (password.length < 8) {
      toast.error('Senha deve ter pelo menos 8 caracteres');
      return;
    }
    if (password !== confirm) {
      toast.error('Senhas não conferem');
      return;
    }

    let mnemonic: string[] | undefined;
    if (mode === 'restore_greenwallet') {
      const words = mnemonicInput.trim().split(/\s+/).filter(Boolean);
      if (words.length !== 24) {
        toast.error('Cole exatamente 24 palavras separadas por espaço');
        return;
      }
      mnemonic = words;
    }

    setSubmitting(true);
    try {
      const resp = await signup({
        name: name.trim(),
        email: email.trim(),
        password,
        mode,
        ...(mnemonic ? { mnemonic } : {}),
        ...(mode === 'external_wallet' ? { wallet_address: walletAddress.trim() } : {}),
      });
      if (resp.mnemonic) {
        // Não logamos ainda: guardamos as credenciais e exibimos o MnemonicGate.
        // O login só acontece após o usuário confirmar que anotou as 24 palavras.
        setPendingSession({ mnemonic: resp.mnemonic, token: resp.token, user: resp.user });
        return;
      }
      toast.success(`Conta criada. Bem-vindo(a), ${resp.user.name}!`);
      navigate('/', { replace: true });
    } catch (err) {
      toast.error(humanizeApiError(err, 'Falha ao criar conta'), { duration: 8000 });
    } finally {
      setSubmitting(false);
    }
  };

  if (pendingSession) {
    return (
      <MnemonicGate
        words={pendingSession.mnemonic}
        onConfirm={() => {
          loginWithToken(pendingSession.token, pendingSession.user);
          setPendingSession(null);
          toast.success(`Conta criada. Bem-vindo(a), ${pendingSession.user.name}!`);
          navigate('/', { replace: true });
        }}
      />
    );
  }

  return (
    <div className="min-h-screen flex items-center justify-center bg-bg px-6 py-12">
      <div className="w-full max-w-md">
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

        <h1 className="text-2xl font-extrabold tracking-tight text-ink mb-1">Criar conta</h1>
        <p className="text-ink-3 text-sm mb-6">
          Cadastre-se como reciclador no sistema.
        </p>

        <form onSubmit={onSubmit} className="space-y-4">
          <div className="space-y-1.5">
            <label className="gt-eyebrow block" htmlFor="name">Nome</label>
            <Input
              id="name"
              autoFocus
              value={name}
              onChange={(e) => setName(e.target.value)}
              disabled={submitting}
              placeholder="nome completo"
            />
          </div>

          <div className="space-y-1.5">
            <label className="gt-eyebrow block" htmlFor="email">Email</label>
            <Input
              id="email"
              type="email"
              autoComplete="email"
              value={email}
              onChange={(e) => setEmail(e.target.value)}
              disabled={submitting}
              placeholder="email@exemplo.com"
            />
          </div>

          <div className="grid grid-cols-2 gap-3">
            <div className="space-y-1.5">
              <label className="gt-eyebrow block" htmlFor="password">Senha</label>
              <Input
                id="password"
                type="password"
                autoComplete="new-password"
                value={password}
                onChange={(e) => setPassword(e.target.value)}
                disabled={submitting}
                placeholder="••••••••"
              />
            </div>
            <div className="space-y-1.5">
              <label className="gt-eyebrow block" htmlFor="confirm">Confirmar</label>
              <Input
                id="confirm"
                type="password"
                autoComplete="new-password"
                value={confirm}
                onChange={(e) => setConfirm(e.target.value)}
                disabled={submitting}
                placeholder="••••••••"
              />
            </div>
          </div>

          <div className="space-y-2">
            <div className="gt-eyebrow">Wallet</div>
            <label className="flex items-start gap-2 cursor-pointer text-sm">
              <input
                type="radio"
                name="mode"
                checked={mode === 'new_greenwallet'}
                onChange={() => setMode('new_greenwallet')}
                disabled={submitting}
                className="mt-1"
              />
              <span>
                <span className="font-semibold text-ink">Criar greenwallet nova</span>
                <span className="block text-ink-4 text-[12px]">
                  Geramos 24 palavras para você anotar. Use essas palavras para restaurar sua wallet.
                </span>
              </span>
            </label>
            <label className="flex items-start gap-2 cursor-pointer text-sm">
              <input
                type="radio"
                name="mode"
                checked={mode === 'restore_greenwallet'}
                onChange={() => setMode('restore_greenwallet')}
                disabled={submitting}
                className="mt-1"
              />
              <span>
                <span className="font-semibold text-ink">Já tenho uma greenwallet</span>
                <span className="block text-ink-4 text-[12px]">
                  Cole as 24 palavras para restaurar a mesma wallet (mantém saldo on-chain).
                </span>
              </span>
            </label>
            <label className="flex items-start gap-2 cursor-pointer text-sm">
              <input
                type="radio"
                name="mode"
                checked={mode === 'external_wallet'}
                onChange={() => setMode('external_wallet')}
                disabled={submitting}
                className="mt-1"
              />
              <span>
                <span className="font-semibold text-ink">Usar wallet externa (ex: Lace)</span>
                <span className="block text-ink-4 text-[12px]">
                  Informe um endereço Cardano que você já possui.
                </span>
              </span>
            </label>
          </div>

          {mode === 'restore_greenwallet' && (
            <div className="space-y-1.5">
              <label className="gt-eyebrow block" htmlFor="mnemonic">24 palavras</label>
              <textarea
                id="mnemonic"
                rows={3}
                value={mnemonicInput}
                onChange={(e) => setMnemonicInput(e.target.value)}
                disabled={submitting}
                placeholder="palavra1 palavra2 palavra3 ... palavra24"
                className="w-full rounded-md border border-input bg-background px-3 py-2 text-sm font-mono"
              />
            </div>
          )}

          {mode === 'external_wallet' && (
            <div className="space-y-1.5">
              <label className="gt-eyebrow block" htmlFor="wallet">Endereço da wallet</label>
              <Input
                id="wallet"
                value={walletAddress}
                onChange={(e) => setWalletAddress(e.target.value)}
                disabled={submitting}
                placeholder="addr_test1..."
              />
            </div>
          )}

          <Button
            type="submit"
            disabled={submitting}
            className="w-full bg-gt-600 hover:bg-gt-700 text-white"
          >
            {submitting ? 'Criando...' : 'Criar conta'}
          </Button>
        </form>

        <div className="mt-6 text-center text-sm text-ink-3">
          Já tem conta?{' '}
          <Link to="/login" className="text-gt-700 font-semibold hover:underline">
            Entrar
          </Link>
        </div>
      </div>
    </div>
  );
}
