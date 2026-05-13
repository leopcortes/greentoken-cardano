import { Link, useNavigate } from 'react-router-dom';
import { toast } from 'sonner';
import { useStation } from './StationContext';
import { LogOut, Wallet } from 'lucide-react';
import { useAuth } from '@/auth/AuthContext';

export function TopBar() {
  const { bottlesProcessed, activeStage, inFlight } = useStation();
  const { logout } = useAuth();
  const navigate = useNavigate();

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
    <div className="flex items-center justify-between gap-4">
      <div className="flex items-center gap-3">
        <div
          className="w-9 h-9 rounded-[10px] bg-gt-600 text-white flex items-center justify-center font-extrabold text-base tracking-[-0.04em]"
          style={{ boxShadow: '0 4px 12px rgba(22,163,74,0.4)' }}
        >
          ₲
        </div>
        <div>
          <div className="text-md font-bold tracking-[-0.01em] leading-snug">Greentoken Station</div>
          <div className="text-[12px] text-ink-3 leading-tight">Container de reciclagem na blockchain Cardano</div>
        </div>
      </div>

      <div className="flex gap-[10px] items-center">
        <span className="gt-chip gt-chip--ghost">
          <span className="mono">{bottlesProcessed}</span> hoje
        </span>
        <span className="gt-chip gt-chip--cdn">Cardano · preprod</span>
        
        <Link
          to="/wallet"
          className="inline-flex items-center gap-[6px] px-3 py-[6px] rounded-lg border border-yellow-300 bg-yellow-100 text-yellow-800 text-[11px] font-semibold no-underline hover:bg-yellow-100 transition-colors"
        >
          <Wallet size={12} />
          Minha carteira
        </Link>

        <button
            type="button"
            onClick={onLogout}
            disabled={pipelineBusy}
            className="inline-flex items-center justify-center gap-1 h-7 px-2.5 rounded-md border border-red-300 bg-red-100 text-red-800 text-[11px] font-semibold hover:bg-red-100 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
            title="Encerrar sessao"
          >
            <LogOut size={12} />
            Sair
          </button>
      </div>
    </div>
  );
}
