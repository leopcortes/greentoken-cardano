import { useCallback, useMemo, type ReactNode } from 'react';
import { toast } from 'sonner';
import {
  ArrowDown,
  ArrowUp,
  ExternalLink,
  QrCode,
  Send,
  Shield,
} from 'lucide-react';
import { Dialog, DialogContent } from '@/components/ui/dialog';
import { CopyButton } from '@/components/ui/copy-button';
import { fmtDateTime } from '@/lib/helpers';
import { STAGE_LABELS, t } from '@/lib/labels';
import { truncateMiddle } from '@/lib/truncate';
import type { Reward, User } from '@/services/api';

export const VOUCHER_RATE = 0.05;
// Cada tx de mint envia 2 ADA (2_000_000 lovelace) junto com o token,
// conforme cardano.service.ts.
export const ADA_PER_REWARD_TX = 2;

// QR placeholder deterministico: 21x21, finder corners + cells derivados do
// address. Substituir por gerador real de QR quando integrar com mobile wallet.
export function fauxQrCells(address: string): number[] {
  if (!address) return Array(21 * 21).fill(0);
  const arr: number[] = [];
  for (let r = 0; r < 21; r++) {
    for (let c = 0; c < 21; c++) {
      const inFinder = (r < 7 && c < 7) || (r < 7 && c > 13) || (r > 13 && c < 7);
      if (inFinder) {
        const fr = r < 7 ? r : r - 14;
        const fc = c < 7 ? c : c - 14;
        const onEdge = fr === 0 || fr === 6 || fc === 0 || fc === 6;
        const inCenter = fr >= 2 && fr <= 4 && fc >= 2 && fc <= 4;
        arr.push(onEdge || inCenter ? 1 : 0);
      } else {
        const k = (address.charCodeAt((r * 21 + c) % address.length) + r * 7 + c * 3) % 7;
        arr.push(k < 3 ? 1 : 0);
      }
    }
  }
  return arr;
}

export function fmtNumber(n: number, decimals = 0): string {
  return n.toLocaleString('pt-BR', {
    minimumFractionDigits: decimals,
    maximumFractionDigits: decimals,
  });
}

export function initialsFor(name: string): string {
  return name
    .split(' ')
    .map((p) => p[0])
    .filter(Boolean)
    .slice(0, 2)
    .join('')
    .toUpperCase();
}

interface QrModalProps {
  open: boolean;
  user: User | null;
  onClose: () => void;
}

export function QrModal({ open, user, onClose }: QrModalProps) {
  const cells = useMemo(() => fauxQrCells(user?.wallet_address ?? ''), [user?.wallet_address]);

  const copyAddress = useCallback(() => {
    if (!user?.wallet_address) return;
    navigator.clipboard.writeText(user.wallet_address).then(() => {
      toast.success('Endereço copiado!', { duration: 3000 });
    }).catch(() => {
      toast.error('Não foi possível copiar o endereço.', { duration: 4000 });
    });
  }, [user?.wallet_address]);

  if (!user || !user.wallet_address) return null;

  return (
    <Dialog open={open} onOpenChange={(o) => { if (!o) onClose(); }}>
      <DialogContent className="max-w-[460px] p-6">
        <div className="mb-4 pr-6">
          <div className="gt-eyebrow">Receber Greentoken / ADA</div>
          <h3 className="text-lg font-bold mt-1">{user.name}</h3>
        </div>

        <div className="flex flex-col items-center mb-4 gap-2">
          <button
            type="button"
            onClick={copyAddress}
            title="Clique para copiar o endereço"
            className="group relative cursor-pointer focus:outline-none"
          >
            <div
              className="grid bg-white p-3 rounded-lg border border-line group-hover:border-gt-400 transition-colors"
              style={{ gridTemplateColumns: 'repeat(21, 1fr)', width: 200, height: 200 }}
            >
              {cells.map((on, i) => (
                <div
                  key={i}
                  style={{ aspectRatio: '1/1', background: on ? 'var(--ink)' : 'transparent' }}
                />
              ))}
            </div>
            <span className="absolute inset-0 flex items-center justify-center rounded-lg bg-white/80 opacity-0 group-hover:opacity-100 transition-opacity text-xs font-semibold text-gt-700">
              Copiar endereço
            </span>
          </button>
          <span className="text-[11px] text-ink-4">Clique no QR para copiar</span>
        </div>

        <div className="bg-bg-elev border border-line rounded-md p-3 mb-3">
          <div className="gt-eyebrow mb-1.5">Endereço Cardano</div>
          <div className="flex items-center gap-1.5">
            <span className="mono text-[11px] text-ink-2 break-all flex-1 leading-relaxed">
              {user.wallet_address}
            </span>
            <CopyButton value={user.wallet_address} direction="bottom" />
          </div>
        </div>

        <div className="flex items-center gap-2 text-[11px] text-ink-3">
          <span className="gt-chip gt-chip--cdn whitespace-nowrap flex-shrink-0">Cardano · preprod</span>
          <span>Envie apenas tokens preprod para este endereço.</span>
        </div>
      </DialogContent>
    </Dialog>
  );
}

interface BalanceCardProps {
  kind: 'gt' | 'ada';
  value: string;
  sub: string;
  badge?: ReactNode;
  onPrimary?: () => void;
  primaryLabel: string;
  primaryIcon: ReactNode;
  secondaryDisabled?: boolean;
  secondaryTip?: string;
  onSecondary?: () => void;
  isLegacy?: boolean;
  totalGreentoken?: number;
}

export function BalanceCard({
  kind,
  value,
  sub,
  badge,
  onPrimary,
  primaryLabel,
  primaryIcon,
  secondaryDisabled,
  secondaryTip,
  onSecondary,
  isLegacy,
  totalGreentoken,
}: BalanceCardProps) {
  const isGT = kind === 'gt';
  return (
    <div
      className={`gt-card relative overflow-hidden p-[22px] ${
        isGT ? 'border-[#dceadf]' : 'border-[#dde4f1]'
      }`}
      style={{
        background: isGT
          ? 'linear-gradient(135deg, #fff 0%, #fff 60%, var(--gt-50) 100%)'
          : 'linear-gradient(135deg, #fff 0%, #fff 60%, var(--cdn-soft) 100%)',
        boxShadow: isGT
          ? '0 1px 0 rgba(14,20,16,.04), 0 6px 18px rgba(22,163,74,.08)'
          : 'var(--sh-2)',
      }}
    >
      <div
        className="absolute mono font-black select-none pointer-events-none"
        style={{
          top: -28,
          right: -18,
          fontSize: 180,
          lineHeight: 1,
          opacity: 0.55,
          color: isGT ? 'var(--gt-100)' : '#e6ecfa',
        }}
      >
        {isGT ? '₲' : 'A'}
      </div>

      <div className="relative flex justify-between items-start">
        <div>
          <div className="flex items-center gap-2">
            <div className="gt-eyebrow">{isGT ? 'Saldo Greentoken' : 'Saldo ADA'}</div>
            {badge}
          </div>
          <div className="flex items-baseline gap-2 mt-2.5">
            <span
              className="mono font-extrabold text-ink leading-none"
              style={{ fontSize: 44, letterSpacing: '-0.03em' }}
            >
              {value}
            </span>
            <span className={`text-[17px] font-bold ${isGT ? 'text-gt-700' : 'text-cdn'}`}>
              {isGT ? '₲' : '₳'}
            </span>
          </div>
          <div className="text-xs text-ink-3 mt-1.5">{sub}</div>
        </div>
      </div>

      {!isGT ? (
        <div className="relative flex gap-2 mt-[18px]">
          <button
            type="button"
            onClick={onPrimary}
            className="inline-flex items-center gap-1.5 bg-gt-600 hover:bg-gt-700 text-white px-3.5 py-2 rounded-md text-[13px] font-semibold transition-colors"
            style={{ boxShadow: '0 2px 6px rgba(22,163,74,.3)' }}
          >
            {primaryIcon}
            {primaryLabel}
          </button>
          <div className="relative group">
            <button
              type="button"
              disabled={secondaryDisabled}
              onClick={onSecondary}
              className="inline-flex items-center gap-1.5 bg-gt-600 hover:bg-gt-700 disabled:bg-line disabled:text-ink-4 disabled:cursor-not-allowed disabled:shadow-none text-white px-3.5 py-2 rounded-md text-[13px] font-semibold transition-colors"
              style={{ boxShadow: '0 2px 6px rgba(22,163,74,.3)' }}
            >
              <Send size={13} />
              Enviar
            </button>
            {secondaryDisabled && secondaryTip && (
              <span className="absolute bottom-[calc(100%+8px)] left-1/2 -translate-x-1/2 bg-ink text-white text-[11px] px-2.5 py-1.5 rounded-md whitespace-nowrap shadow-2 opacity-0 pointer-events-none group-hover:opacity-100 transition-opacity z-50">
                {secondaryTip}
              </span>
            )}
          </div>
        </div>
      ) : (
        <button
          type="button"
          disabled={isLegacy || totalGreentoken === 0}
          onClick={() => toast.warning('Resgate ainda não disponivel.')}
          className="inline-flex items-center gap-1.5 self-start mt-3.5 bg-gt-600 hover:bg-gt-700 disabled:bg-line disabled:text-ink-4 disabled:cursor-not-allowed text-white px-3.5 py-2 rounded-md text-[13px] font-semibold transition-colors"
          style={{ boxShadow: '0 2px 6px rgba(22,163,74,.3)' }}
        >
          <Shield size={13} />
          Resgatar
        </button>
      )}
    </div>
  );
}

export interface TxRow {
  id: string;
  dir: 'in' | 'out';
  stage: string;
  amount: number;
  ada: number;
  hash: string | null;
  date: string;
}

export function rewardsToTxRows(rewards: Reward[]): TxRow[] {
  return rewards.map((r) => ({
    id: r.id,
    dir: 'in',
    stage: r.stage,
    amount: r.greentoken_amount,
    ada: ADA_PER_REWARD_TX,
    hash: r.tx_hash,
    date: r.sent_at,
  }));
}

interface TxTableProps {
  rows: TxRow[];
  hideADA: boolean;
}

export function TxTable({ rows, hideADA }: TxTableProps) {
  if (rows.length === 0) {
    return (
      <div className="text-center py-9 px-4 text-ink-4 text-[13px] italic">
        Nenhuma transação on-chain registrada para este usuário.
      </div>
    );
  }
  return (
    <div className="overflow-x-auto">
      <table className="w-full border-collapse">
        <thead>
          <tr>
            <th className="text-left text-[11px] font-semibold text-ink-3 px-3 py-2.5 border-b border-line w-[110px]">
              Direção
            </th>
            <th className="text-left text-[11px] font-semibold text-ink-3 px-3 py-2.5 border-b border-line">
              Origem
            </th>
            <th className="text-left text-[11px] font-semibold text-ink-3 px-3 py-2.5 border-b border-line">
              Greentoken
            </th>
            {!hideADA && (
              <th className="text-left text-[11px] font-semibold text-ink-3 px-3 py-2.5 border-b border-line">
                ADA
              </th>
            )}
            <th className="text-left text-[11px] font-semibold text-ink-3 px-3 py-2.5 border-b border-line">
              Hash
            </th>
            <th className="text-left text-[11px] font-semibold text-ink-3 px-3 py-2.5 border-b border-line w-[140px]">
              Data
            </th>
          </tr>
        </thead>
        <tbody>
          {rows.map((tx) => {
            const incoming = tx.dir === 'in';
            return (
              <tr key={tx.id} className="hover:bg-bg/60 transition-colors">
                <td className="px-3 py-3 border-b border-line-2 text-[13px]">
                  <span
                    className={`inline-flex items-center gap-1.5 px-2.5 py-[3px] rounded-full text-[11px] font-semibold border ${
                      incoming
                        ? 'bg-gt-50 text-gt-800 border-gt-200'
                        : 'bg-err-soft text-err border-[#fecaca]'
                    }`}
                  >
                    {incoming ? <ArrowDown size={12} /> : <ArrowUp size={12} />}
                    {incoming ? 'Recebido' : 'Enviado'}
                  </span>
                </td>
                <td className="px-3 py-3 border-b border-line-2 text-[13px] text-ink-2 font-medium">
                  Mint · {t(STAGE_LABELS, tx.stage)}
                </td>
                <td className="px-3 py-3 border-b border-line-2">
                  <span className={`mono font-bold text-[13px] ${incoming ? 'text-gt-700' : 'text-err'}`}>
                    {incoming ? '+' : '−'}
                    {tx.amount} ₲
                  </span>
                </td>
                {!hideADA && (
                  <td className="px-3 py-3 border-b border-line-2">
                    <span className="mono text-ink-3 text-xs">
                      +{tx.ada.toFixed(2)} ₳
                    </span>
                  </td>
                )}
                <td className="px-3 py-3 border-b border-line-2">
                  {tx.hash ? (
                    <span className="inline-flex items-center gap-1">
                      <span className="mono text-[11px] text-ink-3">
                        {truncateMiddle(tx.hash, 8, 6)}
                      </span>
                      <CopyButton value={tx.hash} />
                      <a
                        href={`https://preprod.cardanoscan.io/transaction/${tx.hash}`}
                        target="_blank"
                        rel="noreferrer"
                        className="text-ink-4 hover:text-ink-2 inline-flex p-0.5"
                        title="Abrir no Cardanoscan"
                      >
                        <ExternalLink size={11} />
                      </a>
                    </span>
                  ) : (
                    <span className="text-ink-4 text-xs italic">pendente</span>
                  )}
                </td>
                <td className="px-3 py-3 border-b border-line-2 mono text-[11px] text-ink-3">
                  {fmtDateTime(tx.date)}
                </td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

interface IdentityStripProps {
  user: User;
  onShowQr?: () => void;
}

export function IdentityStrip({ user, onShowQr }: IdentityStripProps) {
  return (
    <div className="gt-card flex items-center gap-6 flex-wrap px-[18px] py-[14px]">
      <div className="flex items-center gap-3">
        <div
          className={`w-10 h-10 rounded-full flex items-center justify-center font-bold text-sm ${
            user.role === 'owner' ? 'bg-blue-100 text-blue-700' : 'bg-gt-100 text-gt-800'
          }`}
        >
          {initialsFor(user.name)}
        </div>
        <div>
          <div className="text-sm font-semibold">{user.name}</div>
          <div className="text-[11px] text-ink-3">{user.email}</div>
        </div>
      </div>

      <div className="self-stretch w-px bg-line" />

      <div className="flex-1 min-w-0">
        <div className="gt-eyebrow mb-1">Endereço Cardano</div>
        <div className="flex items-center gap-1.5">
          <span className="mono text-xs text-ink-2 truncate flex-1">
            {user.wallet_address ?? '- sem endereço associado -'}
          </span>
          {user.wallet_address && (
            <>
              <CopyButton value={user.wallet_address} />
              {onShowQr && (
                <button
                  type="button"
                  onClick={onShowQr}
                  className="text-ink-4 hover:text-ink-2 p-1 rounded inline-flex"
                  title="Mostrar QR"
                >
                  <QrCode size={14} />
                </button>
              )}
            </>
          )}
        </div>
      </div>

      <div>
        <div className="gt-eyebrow mb-1">Pubkey hash</div>
        <div className="mono text-[11px] text-ink-3">
          {user.pubkey_hash ? truncateMiddle(user.pubkey_hash, 10, 8) : '-'}
        </div>
      </div>
    </div>
  );
}
