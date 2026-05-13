import { useCallback, useEffect, useMemo, useState } from 'react';
import { toast } from 'sonner';
import { AlertTriangle, Eye, EyeOff, QrCode, RefreshCw, Shield, Wallet } from 'lucide-react';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog';
import { Button } from '@/components/ui/button';
import {
  getGreenwalletBalance,
  getOperatorBalance,
  getUserRewards,
  getUsers,
  initiateGreenwalletMigration,
  confirmGreenwalletMigration,
  cancelGreenwalletMigration,
  type GreenwalletBalance,
  type MigrationInitiated,
  type OperatorBalance,
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
import { CopyButton } from '@/components/ui/copy-button';
import { truncateMiddle } from '@/lib/truncate';

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
  const [seedDialog, setSeedDialog] = useState<MigrationInitiated | null>(null);
  const [seedAcknowledged, setSeedAcknowledged] = useState(false);
  const [migratingUserId, setMigratingUserId] = useState<string | null>(null);
  const [pendingOldBalance, setPendingOldBalance] = useState<number | null>(null);
  const [pendingNewBalance, setPendingNewBalance] = useState<number | null>(null);
  const [refreshingPendingBalance, setRefreshingPendingBalance] = useState(false);
  const [confirmingMigration, setConfirmingMigration] = useState(false);
  const [confirmGuardOpen, setConfirmGuardOpen] = useState(false);

  const [operatorBalance, setOperatorBalance] = useState<OperatorBalance | null>(null);
  const [loadingOperator, setLoadingOperator] = useState(false);
  const [operatorError, setOperatorError] = useState<string | null>(null);

  const reloadOperator = useCallback(() => {
    setLoadingOperator(true);
    setOperatorError(null);
    getOperatorBalance()
      .then((data) => setOperatorBalance(data))
      .catch((err) => {
        setOperatorError(err instanceof Error ? err.message : 'Erro ao consultar carteira operadora');
      })
      .finally(() => setLoadingOperator(false));
  }, []);

  useEffect(() => {
    reloadOperator();
  }, [reloadOperator]);

  useEffect(() => {
    let alive = true;
    setLoadingUsers(true);
    getUsers()
      .then((data) => {
        if (!alive) return;
        setUsers(data);
        if (data.length > 0) {
          setSelectedUserId((data[0]).id);
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

  const handleStartMigration = useCallback(async () => {
    if (!selectedUser) return;
    setMigratingUserId(selectedUser.id);
    try {
      const result = await initiateGreenwalletMigration(selectedUser.id);
      setSeedDialog(result);
      setSeedAcknowledged(false);
      const fresh = await getUsers();
      setUsers(fresh);
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Falha ao iniciar migracao', {
        duration: 10000,
      });
    } finally {
      setMigratingUserId(null);
    }
  }, [selectedUser]);

  const reloadWalletData = useCallback((userId: string) => {
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
  }, []);

  const refreshPendingBalances = useCallback(async () => {
    if (!selectedUser?.has_pending_migration) return;
    setRefreshingPendingBalance(true);
    try {
      const oldBal = selectedUser.wallet_address
        ? await getGreenwalletBalance(selectedUser.id).catch(() => null)
        : null;
      setPendingOldBalance(oldBal?.greentoken ?? 0);
      setPendingNewBalance(0);
    } finally {
      setRefreshingPendingBalance(false);
    }
  }, [selectedUser]);

  const doConfirmMigration = useCallback(async () => {
    if (!selectedUser) return;
    setConfirmingMigration(true);
    try {
      await confirmGreenwalletMigration(selectedUser.id);
      toast.success('Migração concluída!');
      const fresh = await getUsers();
      setUsers(fresh);
      reloadWalletData(selectedUser.id);
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Falha ao concluir migracao', {
        duration: 10000,
      });
    } finally {
      setConfirmingMigration(false);
      setConfirmGuardOpen(false);
    }
  }, [selectedUser, reloadWalletData]);

  const handleConfirmClick = useCallback(() => {
    if (!selectedUser) return;
    if (selectedUser.role === 'recycler' && (pendingOldBalance ?? 0) > 0) {
      setConfirmGuardOpen(true);
      return;
    }
    void doConfirmMigration();
  }, [selectedUser, pendingOldBalance, doConfirmMigration]);

  const handleCancelMigration = useCallback(async () => {
    if (!selectedUser) return;
    if (!window.confirm('Cancelar a migração? A mnemônica gerada será descartada permanentemente.')) {
      return;
    }
    try {
      await cancelGreenwalletMigration(selectedUser.id);
      toast.success('Migração cancelada');
      const fresh = await getUsers();
      setUsers(fresh);
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Falha ao cancelar', { duration: 10000 });
    }
  }, [selectedUser]);

  useEffect(() => {
    if (!selectedUserId) {
      setBalance(null);
      setRewards([]);
      return;
    }
    reloadWalletData(selectedUserId);
  }, [selectedUserId, reloadWalletData]);

  useEffect(() => {
    if (selectedUser?.has_pending_migration) {
      refreshPendingBalances();
    } else {
      setPendingOldBalance(null);
      setPendingNewBalance(null);
    }
  }, [selectedUser?.id, selectedUser?.has_pending_migration, refreshPendingBalances]);

  const isLegacy = selectedUser != null && !selectedUser.has_greenwallet;
  const totalGreentoken = balance
    ? balance.greentoken
    : rewards.reduce((acc, r) => acc + r.greentoken_amount, 0);
  const adaValue = balance ? Number(balance.ada) : 0;
  const voucherValue = (totalGreentoken * VOUCHER_RATE).toFixed(2);
  const txRows = useMemo(() => rewardsToTxRows(rewards), [rewards]);

  const LOW_UTXO_THRESHOLD = 3;

  const isOwner = selectedUser?.role === 'owner';
  const hasWallet = !!selectedUser?.wallet_address;
  const gridCols = isOwner
    ? (hasWallet ? (showADA ? '1fr 1fr 1fr' : '1fr 1fr') : '1fr')
    : (showADA ? '1fr 1fr' : '1fr');

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
                  {!u.has_greenwallet && ' (legado)'}
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
        </div>
      </div>

      {selectedUser && (
        <IdentityStrip user={selectedUser} onShowQr={() => setQrOpen(true)} />
      )}

      {selectedUser && isLegacy && !selectedUser.has_pending_migration && (
        <div
          className="gt-card flex gap-3 items-start px-4 py-3.5"
          style={{ background: 'var(--warn-soft)', borderColor: '#fde68a' }}
        >
          <Shield size={16} className="text-warn flex-none mt-0.5" />
          <div className="flex-1 text-xs text-warn leading-relaxed">
            <strong>Carteira manual sem custódia greenwallet.</strong> Este usuário
            foi criado antes da migração para mnemônica custodiada (greenwallet).
            Você pode gerar uma greenwallet nova para ele agora - o histórico em{' '}
            <span className="mono">rewards</span>, <span className="mono">bottles</span>{' '}
            e <span className="mono">blockchain_txs</span> é preservado.
          </div>
          <Button
            type="button"
            onClick={handleStartMigration}
            disabled={migratingUserId === selectedUser.id}
            className="bg-gt-600 hover:bg-gt-700 text-white text-xs h-8 flex-none"
          >
            <Wallet size={13} className="mr-1.5" />
            {migratingUserId === selectedUser.id
              ? 'Gerando...'
              : selectedUser.role === 'owner'
                ? 'Gerar greenwallet'
                : 'Migrar para greenwallet'}
          </Button>
        </div>
      )}

      {selectedUser?.has_pending_migration && selectedUser.pending_wallet_address && (
        <div className="gt-card px-4 py-3.5">
          <div className="flex justify-between items-start mb-3">
            <div>
              <div className="text-sm font-semibold text-ink leading-tight">
                Migração pendente
              </div>
              <div className="text-[11px] text-ink-3 mt-0.5">
                {selectedUser.role === 'owner'
                  ? 'Owner não precisa transferir tokens - o endereço antigo era da carteira operadora. Você pode concluir agora.'
                  : 'Transfira os GTs do endereço antigo (Lace) para o novo endereço e depois clique em Concluir.'}
              </div>
            </div>
            <button
              type="button"
              onClick={refreshPendingBalances}
              disabled={refreshingPendingBalance}
              className="inline-flex items-center gap-1 bg-gray-100 hover:bg-gray-200 disabled:opacity-50 text-gray-800 border border-line px-2 py-1 rounded text-[11px] font-medium"
            >
              <RefreshCw size={11} className={refreshingPendingBalance ? 'animate-spin' : ''} />
              Atualizar
            </button>
          </div>

          <div className="grid gap-3" style={{ gridTemplateColumns: '1fr 1fr' }}>
            <div className="border border-line rounded-md px-3 py-2">
              <div className="gt-eyebrow mb-1">Endereço antigo</div>
              <div className="mono text-[10px] text-ink-3 break-all leading-tight">
                {selectedUser.wallet_address ?? '(nenhum)'}
              </div>
              <div className="mt-1.5 text-xs">
                Saldo: <strong>{pendingOldBalance ?? '-'} GT</strong>
              </div>
            </div>
            <div
              className="border border-gt-600 rounded-md px-3 py-2"
              style={{ background: 'var(--gt-50, #f0fdf4)' }}
            >
              <div className="gt-eyebrow mb-1">Endereço novo (greenwallet)</div>
              <div className="mono text-[10px] text-ink-3 break-all leading-tight">
                {selectedUser.pending_wallet_address}
              </div>
              <div className="mt-1.5 text-xs">
                Saldo: <strong>{pendingNewBalance ?? '-'} GT</strong>
              </div>
            </div>
          </div>

          {selectedUser.role === 'recycler' && (
            <div className="mt-3 text-[11px] text-ink-3 leading-relaxed">
              <strong>Como concluir:</strong> abra a Lace, envie todos os GTs do endereço
              antigo para o novo, aguarde a confirmação on-chain, então clique em "Concluir migração".
            </div>
          )}

          <div className="mt-3 flex justify-end gap-2">
            <Button
              type="button"
              variant="outline"
              onClick={handleCancelMigration}
              disabled={confirmingMigration}
            >
              Cancelar migração
            </Button>
            <Button
              type="button"
              onClick={handleConfirmClick}
              disabled={confirmingMigration}
              className="bg-gt-600 hover:bg-gt-700 text-white"
            >
              {confirmingMigration ? 'Concluindo...' : 'Concluir migração'}
            </Button>
          </div>
        </div>
      )}

      {selectedUser && (hasWallet || isOwner) && (
        <div className="grid gap-3.5" style={{ gridTemplateColumns: gridCols }}>
          {hasWallet && (
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
          )}
          {hasWallet && showADA && (
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
          {/* Card carteira operadora - somente owner */}
          {isOwner && (
          <div className="gt-card relative overflow-hidden p-[22px]">
            <div className="flex items-center justify-between mb-4">
              <div className="flex items-center gap-2">
                <span className="gt-eyebrow">Carteira Operadora</span>
                <span className="gt-chip gt-chip--ghost">payment.addr</span>
              </div>
              <button
                type="button"
                onClick={reloadOperator}
                disabled={loadingOperator}
                className="inline-flex items-center gap-1 bg-gray-100 hover:bg-gray-200 disabled:opacity-50 text-gray-800 border border-line px-2.5 py-1 rounded text-[11px] font-medium transition-colors"
              >
                <RefreshCw size={11} className={loadingOperator ? 'animate-spin' : ''} />
                Atualizar
              </button>
            </div>

            {operatorError && (
              <div className="flex items-start gap-2 px-3 py-2 rounded-md border border-[#fecaca] text-xs text-err leading-relaxed"
                style={{ background: 'var(--err-soft)' }}
              >
                <AlertTriangle size={13} className="flex-none mt-0.5" />
                <span>Nó indisponível - {operatorError}</span>
              </div>
            )}

            {!operatorError && operatorBalance && (
              <div className="gap-4 flex flex-col" >
                <div className='grid' style={{ gridTemplateColumns: '1fr 1fr' }}>
                  <div>
                    <div className="gt-eyebrow mb-1">Saldo total</div>
                    <div className="flex items-baseline gap-1">
                      <span className="mono font-bold text-2xl text-ink">
                        {fmtNumber(Number(operatorBalance.ada), 2)}
                      </span>
                      <span className="text-sm font-semibold text-cdn">₳</span>
                    </div>
                  </div>
                  <div>
                    <div className="gt-eyebrow mb-1">UTxOs disponíveis</div>
                    <div className="flex items-center gap-2">
                      <span className={`mono font-bold text-2xl ${
                        operatorBalance.ada_only_utxo_count < LOW_UTXO_THRESHOLD
                          ? 'text-warn'
                          : 'text-ink'
                      }`}>
                        {operatorBalance.ada_only_utxo_count}
                      </span>
                      <span className="text-[11px] text-ink-4">ADA-only</span>
                      {operatorBalance.utxo_count > operatorBalance.ada_only_utxo_count && (
                        <span className="text-[11px] text-ink-4">
                          · {operatorBalance.utxo_count - operatorBalance.ada_only_utxo_count} com tokens
                        </span>
                      )}
                    </div>
                    {operatorBalance.ada_only_utxo_count < LOW_UTXO_THRESHOLD && (
                      <div className="flex items-center gap-1 mt-1 text-[10px] text-warn font-medium">
                        <AlertTriangle size={10} />
                        Execute split-utxos.sh
                      </div>
                    )}
                  </div>
                </div>
                <div>
                  <div className="gt-eyebrow mb-1">Endereço</div>
                  <div className="flex items-center gap-1">
                    <span className="mono text-[11px] text-ink-2">
                      {truncateMiddle(operatorBalance.address, 32, 8)}
                    </span>
                    <CopyButton value={operatorBalance.address} />
                  </div>
                </div>
              </div>
            )}

            {!operatorError && !operatorBalance && !loadingOperator && (
              <div className="text-[12px] text-ink-4 italic">Sem dados do nó Cardano.</div>
            )}
          </div>
          )}
        </div>
      )}

      {selectedUser && !selectedUser.wallet_address && !isOwner && (
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
          Saldos derivados diretamente de Blockfrost, independente do banco de dados.
          As recompensas persistem on-chain.
        </div>
      )}

      {!loadingUsers && users.length === 0 && (
        <div className="gt-card text-center py-12 text-ink-4 text-sm">
          Nenhum usuário cadastrado. Crie um usuário na aba <strong>Usuários</strong>.
        </div>
      )}

      <QrModal open={qrOpen} user={selectedUser} onClose={() => setQrOpen(false)} />

      <Dialog open={confirmGuardOpen} onOpenChange={setConfirmGuardOpen}>
        <DialogContent className="max-w-sm">
          <DialogHeader>
            <DialogTitle>Confirmar conclusão</DialogTitle>
          </DialogHeader>
          <p className="text-sm text-ink-2 leading-relaxed">
            O endereço antigo ainda tem <strong>{pendingOldBalance ?? 0} GT</strong>.
            Concluir agora torna esses tokens inacessíveis pelo app.
          </p>
          <p className="text-xs text-ink-3 -mt-1">Tem certeza?</p>
          <div className="flex justify-end gap-2 pt-2">
            <Button
              type="button"
              variant="outline"
              onClick={() => setConfirmGuardOpen(false)}
              disabled={confirmingMigration}
            >
              Voltar
            </Button>
            <Button
              type="button"
              onClick={doConfirmMigration}
              disabled={confirmingMigration}
              className="bg-gt-600 hover:bg-gt-700 text-white"
            >
              {confirmingMigration ? 'Concluindo...' : 'Concluir mesmo assim'}
            </Button>
          </div>
        </DialogContent>
      </Dialog>

      <Dialog
        open={seedDialog !== null}
        onOpenChange={(open) => {
          if (!open) {
            setSeedDialog(null);
            setSeedAcknowledged(false);
          }
        }}
      >
        <DialogContent className="max-w-lg">
          <DialogHeader>
            <DialogTitle>Mnemônica da greenwallet</DialogTitle>
          </DialogHeader>
          {seedDialog && (
            <>
              <p className="text-xs text-ink-3 leading-relaxed">
                Anote estas 24 palavras agora. Elas só serão exibidas nesta janela -
                depois, para vê-las novamente, será preciso usar o endpoint{' '}
                <span className="mono">GET /users/:id/greenwallet/seed</span>.
              </p>

              <div className="grid grid-cols-3 gap-2 my-3 p-3 border border-line rounded-md">
                {seedDialog.mnemonic.map((word, i) => (
                  <div key={i} className="text-xs">
                    <span className="text-ink-4 mono">{(i + 1).toString().padStart(2, '0')}.</span>{' '}
                    <span className="font-medium">{word}</span>
                  </div>
                ))}
              </div>

              <div className="text-[11px] text-ink-3">
                <div>
                  <strong>Endereço novo:</strong>{' '}
                  <span className="mono break-all">{seedDialog.new_address}</span>
                </div>
                {seedDialog.old_address && (
                  <div className="mt-1">
                    <strong>Endereço antigo:</strong>{' '}
                    <span className="mono break-all">{seedDialog.old_address}</span>
                  </div>
                )}
              </div>

              <label className="flex items-center gap-2 mt-3 text-xs cursor-pointer">
                <input
                  type="checkbox"
                  checked={seedAcknowledged}
                  onChange={(e) => setSeedAcknowledged(e.target.checked)}
                />
                Anotei a frase em local seguro.
              </label>

              <div className="flex justify-end gap-2 pt-2">
                <Button
                  type="button"
                  variant="outline"
                  onClick={() => {
                    void navigator.clipboard.writeText(seedDialog.mnemonic.join(' '));
                    toast.success('Mnemônica copiada para o clipboard');
                  }}
                >
                  Copiar
                </Button>
                <Button
                  type="button"
                  onClick={() => {
                    setSeedDialog(null);
                    setSeedAcknowledged(false);
                  }}
                  disabled={!seedAcknowledged}
                  className="bg-gt-600 hover:bg-gt-700 text-white"
                >
                  Concluir
                </Button>
              </div>
            </>
          )}
        </DialogContent>
      </Dialog>
    </div>
  );
}
