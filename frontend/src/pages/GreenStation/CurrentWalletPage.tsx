import { toast } from 'sonner';
import { Button } from '@/components/ui/button';
import { CopyButton } from '@/components/ui/copy-button';
import { useStation } from './StationContext';
import { truncMid } from './helpers';

export function CurrentWalletPage() {
  const { tokens, bumpKey, txLog, walletRef } = useStation();

  const tier = tokens >= 500
    ? { name: 'Ouro', color: '#a16207', soft: '#fef3c7' }
    : tokens >= 200
    ? { name: 'Prata', color: '#475569', soft: '#e2e8f0' }
    : { name: 'Bronze', color: '#9a3412', soft: '#fed7aa' };

  return (
    <div ref={walletRef} className="gt-card p-[18px] flex-1 flex min-h-0 flex-col">
      <div className="flex items-start mb-[14px] justify-between">
        <div>
          <div className="gt-eyebrow">Carteira Cardano</div>
          <div className="mono text-[10px] mt-1 flex gap-1 items-center text-ink-4">
            addr1q9x…m4tu
            <CopyButton value="addr1q9xm4tu" />
          </div>
        </div>
        <div
          className="flex items-center gap-[6px] px-2 py-1 rounded-full text-[10px] font-bold tracking-[0.04em]"
          style={{ background: tier.soft, border: `1px solid ${tier.color}33`, color: tier.color }}
        >
          ◆ {tier.name}
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
          className={`flex-1 min-h-0 flex flex-col gap-[6px] ${
            txLog.length > 3 ? 'overflow-y-scroll gt-always-scrollbar pr-3.5' : 'overflow-y-hidden pr-3.5'
          }`}
        >
            {txLog.length > 0 ? txLog.slice(0, 10).map((tx) => (
              <div key={tx.id} className="flex justify-between items-center gap-2 text-[11px]">
                <div className="flex flex-col gap-[2px] min-w-0">
                  <span className="font-semibold text-ink-2">{tx.label}</span>
                  <span className="mono text-ink-4 text-[9px] truncate">{tx.datetime} - {truncMid(tx.hash, 8, 6)}</span>
                </div>
                <div className="mono font-bold text-gt-700 text-xs flex-none">+{tx.reward}</div>
              </div>
          )) : (
            <div className="text-ink-4 text-[11px] mt-2">Ainda sem recompensas.</div>
          )}
        </div>
      </div>
    </div>
  );
}
