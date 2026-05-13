import { useEffect, useMemo, useState } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { toast } from 'sonner';
import { Eye, EyeOff, LogOut, QrCode, RefreshCw, Shield } from 'lucide-react';
import { useAuth } from '@/auth/AuthContext';
import {
  getGreenwalletBalance,
  getUser,
  getUserRewards,
  type GreenwalletBalance,
  type Reward,
  type User,
} from '@/services/api';
import {
  BalanceCard,
  IdentityStrip,
  QrModal,
  TxTable,
  VOUCHER_RATE,
  fmtNumber,
  rewardsToTxRows,
} from './walletWidgets';

export function WalletPage() {
  const { user: authUser, logout } = useAuth();
  const navigate = useNavigate();

  const userId = authUser?.role === 'recycler' ? authUser.userId : null;

  const [user, setUser] = useState<User | null>(null);
  const [balance, setBalance] = useState<GreenwalletBalance | null>(null);
  const [rewards, setRewards] = useState<Reward[]>([]);
  const [loading, setLoading] = useState(false);
  const [hideBalance, setHideBalance] = useState(false);
  const [showADA, setShowADA] = useState(true);
  const [qrOpen, setQrOpen] = useState(false);

  const reload = (id: string) => {
    setLoading(true);
    Promise.all([
      getUser(id),
      getGreenwalletBalance(id).catch(() => null),
      getUserRewards(id).catch(() => ({ rewards: [] as Reward[], total_greentoken: 0 })),
    ])
      .then(([u, bal, rew]) => {
        setUser(u);
        setBalance(bal);
        setRewards(rew.rewards);
      })
      .catch((err) => {
        toast.error(err instanceof Error ? err.message : 'Erro ao carregar carteira', {
          duration: 10000,
        });
      })
      .finally(() => setLoading(false));
  };

  useEffect(() => {
    if (!userId) return;
    reload(userId);
  }, [userId]);

  const isLegacy = user != null && !user.has_greenwallet;
  const totalGreentoken = balance
    ? balance.greentoken
    : rewards.reduce((acc, r) => acc + r.greentoken_amount, 0);
  const adaValue = balance ? Number(balance.ada) : 0;
  const voucherValue = (totalGreentoken * VOUCHER_RATE).toFixed(2);
  const txRows = useMemo(() => rewardsToTxRows(rewards), [rewards]);

  const handleLogout = () => {
    logout();
    navigate('/', { replace: true });
  };

  return (
    <div className="min-h-screen bg-bg">
      <header className="border-b border-line bg-bg-card px-6 py-4 flex items-center justify-between">
        <Link to="/" className="flex items-center gap-3 hover:opacity-80 transition-opacity">
          <div
            className="w-9 h-9 rounded-[10px] bg-gt-600 text-white flex items-center justify-center font-extrabold text-base tracking-[-0.04em]"
            style={{ boxShadow: '0 4px 12px rgba(22,163,74,0.4)' }}
          >
            ₲
          </div>
          <div>
            <div className="text-md font-bold tracking-[-0.01em] leading-snug">
              Minha Greenwallet
            </div>
            <div className="text-[12px] text-ink-3 leading-tight">
              Suas recompensas na blockchain Cardano
            </div>
          </div>
        </Link>
        <div className="flex items-center gap-2">
          <span className="gt-chip gt-chip--cdn">Cardano · preprod</span>
          <button
            type="button"
            onClick={handleLogout}
            className="inline-flex items-center gap-1.5 px-3 py-1.5 rounded-md border border-red-300 bg-red-50 text-red-700 text-[12px] font-semibold hover:bg-red-100 transition-colors"
          >
            <LogOut size={13} />
            Sair
          </button>
        </div>
      </header>

      <main className="mx-auto 2xl:max-w-[75%] lg:max-w-[90%] p-6 space-y-3.5">
        <div className="flex justify-between items-end gap-3 flex-wrap">
          <div>
            <h2 className="text-[22px] font-bold leading-tight" style={{ letterSpacing: '-0.01em' }}>
              {user ? `Ola, ${user.name}` : 'Carregando...'}
            </h2>
            <div className="flex items-center gap-2.5 mt-1.5 text-xs text-ink-3 flex-wrap">
              <span className="gt-chip gt-chip--cdn">
                <span className="gt-pulse mr-1" style={{ background: 'var(--cdn)', width: 6, height: 6 }} />
                Cardano · preprod
              </span>
            </div>
          </div>

          <div className="flex gap-2 items-center flex-wrap">
            <button
              type="button"
              onClick={() => setHideBalance((v) => !v)}
              className="inline-flex items-center gap-1.5 bg-gray-100 hover:bg-gray-200 text-gray-800 border border-line px-3 py-1.5 rounded-md text-xs font-medium transition-colors"
            >
              {hideBalance ? <Eye size={13} /> : <EyeOff size={13} />}
              {hideBalance ? 'Mostrar saldos' : 'Esconder saldos'}
            </button>
            <button
              type="button"
              onClick={() => setShowADA((v) => !v)}
              className="inline-flex items-center gap-1.5 bg-gray-100 hover:bg-gray-200 text-gray-800 border border-line px-3 py-1.5 rounded-md text-xs font-medium transition-colors"
            >
              {showADA ? 'Ocultar ADA' : 'Mostrar ADA'}
            </button>
          </div>
        </div>

        {user && <IdentityStrip user={user} onShowQr={() => setQrOpen(true)} />}

        {user && isLegacy && user.wallet_address && (
          <div
            className="gt-card flex gap-2.5 items-start px-4 py-3"
            style={{ background: 'var(--warn-soft)', borderColor: '#fde68a' }}
          >
            <Shield size={16} className="text-warn flex-none mt-0.5" />
            <div className="text-xs text-warn leading-relaxed">
              <strong>Carteira manual sem custodia greenwallet.</strong> Saldos on-chain
              sao exibidos somente leitura - nao ha possibilidade de resgate de vouchers
              pelo sistema.
            </div>
          </div>
        )}

        {user && user.wallet_address && (
          <div
            className="grid gap-3.5"
            style={{ gridTemplateColumns: showADA ? '1fr 1fr' : '1fr' }}
          >
            <BalanceCard
              kind="gt"
              value={hideBalance ? '••••' : fmtNumber(totalGreentoken)}
              sub={`= R$ ${voucherValue} em vouchers (R$ 0,05 / ₲)`}
              badge={
                balance ? (
                  <span className="gt-chip gt-chip--green">policy ✓</span>
                ) : (
                  <span className="gt-chip gt-chip--ghost">DB</span>
                )
              }
              onPrimary={() => setQrOpen(true)}
              primaryLabel="Receber"
              primaryIcon={<QrCode size={13} />}
              secondaryDisabled
              secondaryTip="Em breve"
              isLegacy={isLegacy}
              totalGreentoken={totalGreentoken}
            />
            {showADA && (
              <BalanceCard
                kind="ada"
                value={hideBalance ? '••••' : fmtNumber(adaValue, 2)}
                sub={`= ${fmtNumber(adaValue * 1_000_000, 0)} lovelace`}
                badge={<span className="gt-chip gt-chip--cdn">on-chain</span>}
                onPrimary={() => setQrOpen(true)}
                primaryLabel="Receber"
                primaryIcon={<QrCode size={13} />}
                secondaryDisabled
                secondaryTip="Em breve"
              />
            )}
          </div>
        )}

        {user && !user.wallet_address && (
          <div
            className="gt-card flex gap-2.5 items-start px-4 py-3"
            style={{ background: 'var(--warn-soft)', borderColor: '#fde68a' }}
          >
            <Shield size={16} className="text-warn flex-none mt-0.5" />
            <div className="text-xs text-warn leading-relaxed">
              <strong>Sem endereco Cardano associado.</strong> Voce ainda nao possui
              uma greenwallet para receber recompensas.
            </div>
          </div>
        )}

        {user && user.wallet_address && (
          <div className="gt-card">
            <div className="flex justify-between items-center px-[18px] py-3.5 border-b border-line">
              <div>
                <h3 className="text-sm font-semibold leading-tight">Transacoes on-chain</h3>
                <p className="text-[11px] text-ink-3 mt-0.5">
                  Recompensas mintadas por estagio ·{' '}
                  {txRows.length === 0
                    ? 'Nenhuma transacao'
                    : txRows.length === 1
                      ? '1 transacao'
                      : `${txRows.length} transacoes`}
                </p>
              </div>
              <button
                type="button"
                onClick={() => userId && reload(userId)}
                disabled={loading}
                className="inline-flex items-center gap-1.5 bg-gray-100 hover:bg-gray-200 disabled:opacity-50 text-gray-800 border border-line px-3 py-1.5 rounded-md text-xs font-medium transition-colors"
              >
                <RefreshCw size={12} className={loading ? 'animate-spin' : ''} />
                Atualizar
              </button>
            </div>
            <TxTable rows={txRows} hideADA={!showADA} />
          </div>
        )}

        <QrModal open={qrOpen} user={user} onClose={() => setQrOpen(false)} />
      </main>
    </div>
  );
}
