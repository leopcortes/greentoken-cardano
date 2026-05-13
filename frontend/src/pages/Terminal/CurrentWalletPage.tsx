import { Link, useNavigate } from 'react-router-dom';
import { toast } from 'sonner';
import { LogOut, Wallet } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { CopyButton } from '@/components/ui/copy-button';
import { useStation } from './StationContext';
import { useAuth } from '@/auth/AuthContext';
import { truncMid } from '@/lib/helpers';

export function CurrentWalletPage() {
  const { tokens, bumpKey, txLog, walletRef, currentUser, activeStage, inFlight } = useStation();
  const { logout } = useAuth();
  const navigate = useNavigate();

  // Bloqueia logout enquanto há garrafas em voo (confirmacoes on-chain ainda
  // referenciam o user_id atual). Mostramos um toast explicando.
  const pipelineBusy = activeStage >= 0 || inFlight.length > 0;

  const onLogout = () => {
    if (pipelineBusy) {
      toast.warning('Aguarde as garrafas em andamento finalizarem antes de sair.', { duration: 6000 });
      return;
    }
    logout();
    navigate('/', { replace: true });
  };

  return (
    <div ref={walletRef} className="gt-card p-[18px] flex-1 flex min-h-0 flex-col">
      <div className="flex items-start mb-[14px] justify-between gap-3">
        <div className="min-w-0 flex-1">
          <div className="gt-eyebrow">Carteira Cardano</div>
          <div className="text-sm font-semibold text-ink mt-1 truncate">
            {currentUser?.name ?? '...'}
          </div>
          <div className="mono text-[10px] mt-0.5 flex gap-1 items-center text-ink-4">
            {currentUser?.wallet_address ? truncMid(currentUser.wallet_address, 14, 6) : '-'}
            {currentUser?.wallet_address && <CopyButton value={currentUser.wallet_address} />}
          </div>
        </div>
        <div className="flex flex-col gap-1.5 flex-none">
          <Link
            to="/wallet"
            className="inline-flex items-center justify-center gap-1 h-7 px-2.5 rounded-md border border-yellow-300 bg-yellow-100 text-yellow-800 text-[10px] font-semibold no-underline hover:bg-yellow-100 transition-colors"
            title="Ver carteira completa"
          >
            <Wallet size={11} />
            Carteira
          </Link>
          <button
            type="button"
            onClick={onLogout}
            disabled={pipelineBusy}
            className="inline-flex items-center justify-center gap-1 h-7 px-2.5 rounded-md border border-red-300 bg-red-100 text-red-800 text-[10px] font-semibold hover:bg-red-100 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
            title="Encerrar sessao"
          >
            <LogOut size={11} />
            Sair
          </button>
        </div>
      </div>

      <div className="flex justify-between items-end mb-3">
        <div className="flex flex-col">
          <div className="flex items-baseline gap-2">
            <span
              key={bumpKey}
              className="gt-bump text-[44px] font-extrabold tracking-[-0.03em] text-ink font-mono inline-block leading-none"
              style={{ animation: 'gt-bump 0.5s ease-out' }}
            >
              {tokens}
            </span>
            <span className="text-base font-bold text-gt-700">₲</span>
          </div>
          <div className="text-[11px] text-ink-3">
            ≈ R$ {(tokens * 0.05).toFixed(2)} em vouchers
          </div>
        </div>

        <Button
          size="sm"
          className="bg-gt-600 hover:bg-gt-700 text-white text-[11px] font-semibold px-4 h-8"
          style={{ boxShadow: '0 2px 6px rgba(22,163,74,0.3)' }}
          onClick={() => toast.warning('Essa função ainda não está disponível!')}
        >
          Resgatar
        </Button>
      </div>

      <div className="pt-3 flex-1 min-h-0 flex flex-col border-t border-line">
        <div className="gt-eyebrow mb-2">Últimas recompensas</div>
        <div
          className={`flex-1 min-h-0 flex flex-col gap-[6px] pl-1 ${
            txLog.length > 3 ? 'overflow-y-scroll gt-always-scrollbar pr-2' : 'overflow-y-hidden pr-3.5'
          }`}
        >
          {txLog.length === 0 ? (
            <div className="flex-1 flex items-center justify-center text-[11px] text-ink-4 italic">
              Sem recompensas ainda.
            </div>
          ) : (
            txLog.slice(0, 10).map((tx) => (
              <div key={tx.id} className="flex justify-between items-center gap-2 text-[11px]">
                <div className="flex flex-col gap-[2px] min-w-0">
                  <span className="font-semibold text-ink-2">{tx.label}</span>
                  <span className="mono text-ink-4 text-[9px] truncate">
                    {tx.datetime}{tx.hash ? ` - ${truncMid(tx.hash, 8, 6)}` : ''}
                  </span>
                </div>
                <div className="mono font-bold text-gt-700 text-xs flex-none">+{tx.reward}</div>
              </div>
            ))
          )}
        </div>
      </div>
    </div>
  );
}
