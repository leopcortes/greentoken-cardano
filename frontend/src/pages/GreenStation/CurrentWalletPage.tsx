import { toast } from 'sonner';
import { Button } from '@/components/ui/button';
import { CopyButton } from '@/components/ui/copy-button';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import { useStation } from './StationContext';
import { truncMid } from '@/lib/helpers';

export function CurrentWalletPage() {
  const {
    tokens, ada, bumpKey, txLog, walletRef,
    users, currentUser, currentUserId, setCurrentUserId,
    activeStage,
  } = useStation();
  const pipelineBusy = activeStage >= 0;
  const adaDisplay = ada !== null ? Number(ada).toFixed(2) : null;

  return (
    <div ref={walletRef} className="gt-card p-[18px] flex-1 flex min-h-0 flex-col">
      <div className="flex items-start mb-[14px] justify-between gap-3">
        <div className="min-w-0 flex-1">
          <div className="gt-eyebrow">Carteira Cardano</div>
          <div className="text-sm font-semibold text-ink mt-1 truncate">
            {currentUser?.name ?? 'Nenhum reciclador'}
          </div>
          <div className="mono text-[10px] mt-0.5 flex gap-1 items-center text-ink-4">
            {currentUser?.wallet_address ? truncMid(currentUser.wallet_address, 8, 6) : '-'}
            {currentUser?.wallet_address && <CopyButton value={currentUser.wallet_address} />}
          </div>
        </div>
        <Select
          value={currentUserId ?? undefined}
          onValueChange={(v) => setCurrentUserId(v)}
          disabled={pipelineBusy}
        >
          <SelectTrigger className="h-8 w-[150px] text-[11px] flex-none">
            <SelectValue placeholder="Selecionar" />
          </SelectTrigger>
          <SelectContent>
            {users.length === 0 ? (
              <div className="px-3 py-4 text-xs text-center text-muted-foreground">
                Nenhum reciclador
              </div>
            ) : (
              users.map((u) => (
                <SelectItem key={u.id} value={u.id} className="text-[11px]">
                  {u.name}
                </SelectItem>
              ))
            )}
          </SelectContent>
        </Select>
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
          {adaDisplay !== null && (
            <div className="text-[11px] text-ink-3 mt-1 mono">
              {adaDisplay} ADA on-chain
            </div>
          )}
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
