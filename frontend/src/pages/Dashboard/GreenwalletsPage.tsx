import { useEffect, useMemo, useState } from 'react';
import { toast } from 'sonner';
import {
  ArrowDown,
  ArrowUp,
  ExternalLink,
  Eye,
  EyeOff,
  QrCode,
  RefreshCw,
  Send,
  Shield,
} from 'lucide-react';
import { Dialog, DialogContent } from '@/components/ui/dialog';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import { CopyButton } from '@/components/ui/copy-button';
import {
  getGreenwalletBalance,
  getUserRewards,
  getUsers,
  type GreenwalletBalance,
  type Reward,
  type User,
} from '@/services/api';
import { fmtDateTime } from '@/lib/helpers';
import { STAGE_LABELS, t } from '@/lib/labels';
import { truncateMiddle } from '@/lib/truncate';

const VOUCHER_RATE = 0.05;
// Cada tx de mint envia 2 ADA (2_000_000 lovelace) junto com o token,
// conforme cardano.service.ts (`--tx-out '<addr>+2000000 + N <policy>.Greentoken'`).
const ADA_PER_REWARD_TX = 2;

// QR placeholder deterministico (mesma logica do design):
// 21x21, finder corners + cells derivados do address.
function fauxQrCells(address: string): number[] {
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

function fmtNumber(n: number, decimals = 0): string {
  return n.toLocaleString('pt-BR', {
    minimumFractionDigits: decimals,
    maximumFractionDigits: decimals,
  });
}

function initialsFor(name: string): string {
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

function QrModal({ open, user, onClose }: QrModalProps) {
  const cells = useMemo(() => fauxQrCells(user?.wallet_address ?? ''), [user?.wallet_address]);
  if (!user || !user.wallet_address) return null;

  return (
    <Dialog open={open} onOpenChange={(o) => { if (!o) onClose(); }}>
      <DialogContent className="max-w-[460px] p-6">
        <div className="mb-4 pr-6">
          <div className="gt-eyebrow">Receber Greentoken / ADA</div>
          <h3 className="text-lg font-bold mt-1">{user.name}</h3>
        </div>

        <div className="flex justify-center mb-4">
          <div
            className="grid bg-white p-3 rounded-lg border border-line"
            style={{ gridTemplateColumns: 'repeat(21, 1fr)', width: 200, height: 200 }}
          >
            {cells.map((on, i) => (
              <div
                key={i}
                style={{ aspectRatio: '1/1', background: on ? 'var(--ink)' : 'transparent' }}
              />
            ))}
          </div>
        </div>

        <div className="bg-bg-elev border border-line rounded-md p-3 mb-3">
          <div className="gt-eyebrow mb-1.5">Endereço Cardano</div>
          <div className="flex items-center gap-1.5">
            <span className="mono text-[11px] text-ink-2 break-all flex-1 leading-relaxed">
              {user.wallet_address}
            </span>
            <CopyButton value={user.wallet_address} />
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
  badge?: React.ReactNode;
  onPrimary?: () => void;
  primaryLabel: string;
  primaryIcon: React.ReactNode;
  secondaryDisabled?: boolean;
  secondaryTip?: string;
  onSecondary?: () => void;
  isLegacy?: boolean;
  totalGreentoken?: number;
}

function BalanceCard({
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
      {/* glyph decorativo de fundo */}
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
            <span
              className={`text-[17px] font-bold ${isGT ? 'text-gt-700' : 'text-cdn'}`}
            >
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
          onClick={() => toast.warning('Resgate ainda não disponível.')}
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

interface TxRow {
  id: string;
  dir: 'in' | 'out';
  stage: string;
  amount: number;
  ada: number;
  hash: string | null;
  date: string;
}

function rewardsToTxRows(rewards: Reward[]): TxRow[] {
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

function TxTable({ rows, hideADA }: TxTableProps) {
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
                  <span
                    className={`mono font-bold text-[13px] ${
                      incoming ? 'text-gt-700' : 'text-err'
                    }`}
                  >
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

export function GreenwalletsPage() {
  const [users, setUsers] = useState<User[]>([]);
  const [selectedUserId, setSelectedUserId] = useState<string | null>(null);
  const [balance, setBalance] = useState<GreenwalletBalance | null>(null);
  const [rewards, setRewards] = useState<Reward[]>([]);
  const [loadingUsers, setLoadingUsers] = useState(true);
  const [loadingBalance, setLoadingBalance] = useState(false);
  const [hideBalance, setHideBalance] = useState(false);
  const [showADA, setShowADA] = useState(true);
  const [qrOpen, setQrOpen] = useState(false);

  useEffect(() => {
    let alive = true;
    setLoadingUsers(true);
    getUsers()
      .then((data) => {
        if (!alive) return;
        setUsers(data);
        if (data.length > 0) {
          // Default: primeiro recycler; se nao houver, primeiro user
          const firstRecycler = data.find((u) => u.role === 'recycler');
          setSelectedUserId((firstRecycler ?? data[0]).id);
        }
      })
      .catch((err) => {
        toast.error(err instanceof Error ? err.message : 'Erro ao carregar usuários', {
          duration: 10000,
        });
      })
      .finally(() => {
        if (alive) setLoadingUsers(false);
      });
    return () => { alive = false; };
  }, []);

  const selectedUser = useMemo(
    () => users.find((u) => u.id === selectedUserId) ?? null,
    [users, selectedUserId],
  );

  const reloadWalletData = (userId: string) => {
    setLoadingBalance(true);
    Promise.all([
      getGreenwalletBalance(userId).catch(() => null),
      getUserRewards(userId).catch(() => ({ rewards: [] as Reward[], total_greentoken: 0 })),
    ])
      .then(([bal, rew]) => {
        setBalance(bal);
        setRewards(rew.rewards);
      })
      .finally(() => setLoadingBalance(false));
  };

  useEffect(() => {
    if (!selectedUserId) {
      setBalance(null);
      setRewards([]);
      return;
    }
    reloadWalletData(selectedUserId);
  }, [selectedUserId]);

  // Legado: user com wallet_address informado manualmente (ex.: criado direto na
  // Lace antes da migracao greenwallet) e sem mnemonica custodiada. Diferente de
  // "sem endereco" - legados ainda tem saldo on-chain consultavel, so nao tem
  // custodia/assinatura no sistema.
  const isLegacy = selectedUser != null && !selectedUser.has_greenwallet;

  // Saldo: vem do backend (Blockfrost). Fallback para soma de rewards do DB.
  const totalGreentoken = balance
    ? balance.greentoken
    : rewards.reduce((acc, r) => acc + r.greentoken_amount, 0);
  const adaValue = balance ? Number(balance.ada) : 0;
  const voucherValue = (totalGreentoken * VOUCHER_RATE).toFixed(2);
  const txRows = useMemo(() => rewardsToTxRows(rewards), [rewards]);

  return (
    <div className="space-y-3.5">
      {/* PAGE HEADER */}
      <div className="flex justify-between items-end gap-3 flex-wrap">
        <div>
          <h2 className="text-[22px] font-bold leading-tight" style={{ letterSpacing: '-0.01em' }}>
            {selectedUser ? `Carteira de ${selectedUser.name}` : 'Greenwallets'}
          </h2>
          <div className="flex items-center gap-2.5 mt-1.5 text-xs text-ink-3 flex-wrap">
            <span className="gt-chip gt-chip--cdn">
              <span className="gt-pulse mr-1" style={{ background: 'var(--cdn)', width: 6, height: 6 }} />
              Cardano · preprod
            </span>
            {/* <span>·</span>
            <span>
              Custódia <strong className="text-ink-2">greenwallet</strong> (BIP-39 / 24 palavras)
            </span>
            {isLegacy && (
              <span className="inline-flex items-center gap-1 text-[10px] font-semibold tracking-[0.04em] uppercase bg-warn-soft text-warn px-1.5 py-0.5 rounded-full">
                Legacy · sem mnemônica
              </span>
            )} */}
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
          <Select
            value={selectedUserId ?? undefined}
            onValueChange={(v) => setSelectedUserId(v)}
            disabled={loadingUsers}
          >
            <SelectTrigger className="h-9 w-[200px] text-[13px]">
              <SelectValue placeholder={loadingUsers ? 'Carregando...' : 'Selecionar carteira'} />
            </SelectTrigger>
            <SelectContent>
              {users.map((u) => (
                <SelectItem key={u.id} value={u.id} className="text-[13px]">
                  {u.name}
                  {!u.has_greenwallet && ' (legacy)'}
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
        </div>
      </div>

      {/* IDENTITY STRIP */}
      {selectedUser && (
        <div className="gt-card flex items-center gap-6 flex-wrap px-[18px] py-[14px]">
          <div className="flex items-center gap-3">
            <div
              className={`w-10 h-10 rounded-full flex items-center justify-center font-bold text-sm ${
                selectedUser.role === 'owner'
                  ? 'bg-blue-100 text-blue-700'
                  : 'bg-gt-100 text-gt-800'
              }`}
            >
              {initialsFor(selectedUser.name)}
            </div>
            <div>
              <div className="text-sm font-semibold">{selectedUser.name}</div>
              <div className="text-[11px] text-ink-3">{selectedUser.email}</div>
            </div>
          </div>

          <div className="self-stretch w-px bg-line" />

          <div className="flex-1 min-w-0">
            <div className="gt-eyebrow mb-1">Endereço Cardano</div>
            <div className="flex items-center gap-1.5">
              <span className="mono text-xs text-ink-2 truncate flex-1">
                {selectedUser.wallet_address ?? '- sem endereço associado -'}
              </span>
              {selectedUser.wallet_address && (
                <>
                  <CopyButton value={selectedUser.wallet_address} />
                  <button
                    type="button"
                    onClick={() => setQrOpen(true)}
                    className="text-ink-4 hover:text-ink-2 p-1 rounded inline-flex"
                    title="Mostrar QR"
                  >
                    <QrCode size={14} />
                  </button>
                </>
              )}
            </div>
          </div>

          <div>
            <div className="gt-eyebrow mb-1">Pubkey hash</div>
            <div className="mono text-[11px] text-ink-3">
              {selectedUser.pubkey_hash
                ? truncateMiddle(selectedUser.pubkey_hash, 10, 8)
                : '-'}
            </div>
          </div>
        </div>
      )}

      {/* LEGACY WARNING - user com endereco da Lace, sem mnemonica custodiada */}
      {selectedUser && isLegacy && selectedUser.wallet_address && (
        <div
          className="gt-card flex gap-2.5 items-start px-4 py-3"
          style={{ background: 'var(--warn-soft)', borderColor: '#fde68a' }}
        >
          <Shield size={16} className="text-warn flex-none mt-0.5" />
          <div className="text-xs text-warn leading-relaxed">
            <strong>Carteira manual sem custódia greenwallet.</strong> Este usuário
            foi criado com endereço Cardano informado manualmente (ex.: extensão
            Lace), antes da migração para mnemônica custodiada. Saldos on-chain
            são exibidos somente leitura - não há assinatura via backend nem
            possibilidade de resgate de vouchers pelo sistema.
          </div>
        </div>
      )}

      {/* BALANCE CARDS GRID */}
      {selectedUser && selectedUser.wallet_address && (
        <div
          className="grid gap-3.5"
          style={{ gridTemplateColumns: showADA ? '1fr 1fr' : '1fr' }}
        >
          <BalanceCard
            kind="gt"
            value={hideBalance ? '••••' : fmtNumber(totalGreentoken)}
            sub={`≈ R$ ${voucherValue} em vouchers (R$ 0,05 / ₲)`}
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

      {/* SEM ENDERECO - user sem wallet_address algum */}
      {selectedUser && !selectedUser.wallet_address && (
        <div
          className="gt-card flex gap-2.5 items-start px-4 py-3"
          style={{ background: 'var(--warn-soft)', borderColor: '#fde68a' }}
        >
          <Shield size={16} className="text-warn flex-none mt-0.5" />
          <div className="text-xs text-warn leading-relaxed">
            <strong>Sem endereço Cardano associado.</strong> Este usuário não
            possui carteira para receber recompensas. Saldos e transações on-chain
            não estão disponíveis.
          </div>
        </div>
      )}

      {/* TX HISTORY */}
      {selectedUser && selectedUser.wallet_address && (
        <div className="gt-card">
          <div className="flex justify-between items-center px-[18px] py-3.5 border-b border-line">
            <div>
              <h3 className="text-sm font-semibold leading-tight">Transações on-chain</h3>
              <p className="text-[11px] text-ink-3 mt-0.5">
                Recompensas mintadas por estágio · {txRows.length === 0 ? 'Nenhuma transação' : (txRows.length === 1 ? '1 transação' : `${txRows.length} transações`)}
              </p>
            </div>
            <div className="flex gap-1.5">
              <button
                type="button"
                onClick={() => selectedUserId && reloadWalletData(selectedUserId)}
                disabled={loadingBalance}
                className="inline-flex items-center gap-1.5 bg-gray-100 hover:bg-gray-200 disabled:opacity-50 text-gray-800 border border-line px-3 py-1.5 rounded-md text-xs font-medium transition-colors"
              >
                <RefreshCw size={12} className={loadingBalance ? 'animate-spin' : ''} />
                Atualizar
              </button>
            </div>
          </div>
          <TxTable rows={txRows} hideADA={!showADA} />
        </div>
      )}

      {selectedUser && selectedUser.wallet_address && (
        <div className="text-[11px] text-ink-4 text-center">
          Saldos derivados diretamente de Blockfrost - independente do banco de dados.
          Recompensas persistem on-chain mesmo se a tabela <span className="mono">rewards</span> for recriada.
        </div>
      )}

      {!loadingUsers && users.length === 0 && (
        <div className="gt-card text-center py-12 text-ink-4 text-sm">
          Nenhum usuário cadastrado. Crie um usuário na aba <strong>Usuários</strong>.
        </div>
      )}

      <QrModal open={qrOpen} user={selectedUser} onClose={() => setQrOpen(false)} />
    </div>
  );
}
