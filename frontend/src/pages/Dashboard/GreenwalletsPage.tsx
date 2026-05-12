import { useEffect, useMemo, useState } from 'react';
import { toast } from 'sonner';
import { Eye, EyeOff, QrCode, RefreshCw, Shield } from 'lucide-react';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import {
  getGreenwalletBalance,
  getUserRewards,
  getUsers,
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
} from '@/pages/Wallet/walletWidgets';

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

  const isLegacy = selectedUser != null && !selectedUser.has_greenwallet;
  const totalGreentoken = balance
    ? balance.greentoken
    : rewards.reduce((acc, r) => acc + r.greentoken_amount, 0);
  const adaValue = balance ? Number(balance.ada) : 0;
  const voucherValue = (totalGreentoken * VOUCHER_RATE).toFixed(2);
  const txRows = useMemo(() => rewardsToTxRows(rewards), [rewards]);

  return (
    <div className="space-y-3.5">
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

      {selectedUser && (
        <IdentityStrip user={selectedUser} onShowQr={() => setQrOpen(true)} />
      )}

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

      {selectedUser && selectedUser.wallet_address && (
        <div className="gt-card">
          <div className="flex justify-between items-center px-[18px] py-3.5 border-b border-line">
            <div>
              <h3 className="text-sm font-semibold leading-tight">Transações on-chain</h3>
              <p className="text-[11px] text-ink-3 mt-0.5">
                Recompensas mintadas por estágio · {txRows.length === 0 ? 'Nenhuma transação' : (txRows.length === 1 ? '1 transação' : `${txRows.length} transações`)}
              </p>
            </div>
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
