import {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useRef,
  useState,
  type ReactNode,
  type RefObject,
} from 'react';
import { toast } from 'sonner';
import type {
  AiResult,
  DragState,
  InventoryBottleData,
  Stage,
  TxLogEntry,
} from './types';
import {
  STAGES,
  buildInventory,
  fmtDateTime,
  makeReplacementBottle,
  rewardToTxLog,
  volumeMlOf,
} from './helpers';
import { Bottle } from '@/components/Bottle';
import {
  createBottle,
  getBottle,
  getContainers,
  getUserRewards,
  getUsers,
  type Container,
  type User,
} from '@/services/api';

interface ConfettiBurst {
  id: string;
  ox: number;
  oy: number;
  tx: number;
  ty: number;
}

interface PickMeta {
  offsetX: number;
  offsetY: number;
  startX: number;
  startY: number;
}

interface StationState {
  inventory: InventoryBottleData[];
  setInventory: React.Dispatch<React.SetStateAction<InventoryBottleData[]>>;

  dragging: DragState | null;
  setDragging: React.Dispatch<React.SetStateAction<DragState | null>>;
  dropArmed: boolean;
  setDropArmed: React.Dispatch<React.SetStateAction<boolean>>;

  lidOpen: boolean;
  setLidOpen: React.Dispatch<React.SetStateAction<boolean>>;
  scanning: boolean;
  setScanning: React.Dispatch<React.SetStateAction<boolean>>;
  reject: boolean;
  setReject: React.Dispatch<React.SetStateAction<boolean>>;

  crushed: number;
  setCrushed: React.Dispatch<React.SetStateAction<number>>;
  fillPct: number;
  setFillPct: React.Dispatch<React.SetStateAction<number>>;
  bottlesProcessed: number;
  setBottlesProcessed: React.Dispatch<React.SetStateAction<number>>;

  activeStage: number;
  setActiveStage: React.Dispatch<React.SetStateAction<number>>;
  completed: Set<Stage>;
  setCompleted: React.Dispatch<React.SetStateAction<Set<Stage>>>;
  currentBottle: InventoryBottleData | null;
  setCurrentBottle: React.Dispatch<React.SetStateAction<InventoryBottleData | null>>;
  aiResult: AiResult;
  setAiResult: React.Dispatch<React.SetStateAction<AiResult>>;

  tokens: number;
  setTokens: React.Dispatch<React.SetStateAction<number>>;
  bumpKey: number;
  setBumpKey: React.Dispatch<React.SetStateAction<number>>;

  txLog: TxLogEntry[];
  setTxLog: React.Dispatch<React.SetStateAction<TxLogEntry[]>>;

  binRef: RefObject<HTMLDivElement>;
  walletRef: RefObject<HTMLDivElement>;

  users: User[];
  containers: Container[];
  setContainers: React.Dispatch<React.SetStateAction<Container[]>>;
  currentUserId: string | null;
  setCurrentUserId: React.Dispatch<React.SetStateAction<string | null>>;
  currentContainerId: string | null;
  setCurrentContainerId: React.Dispatch<React.SetStateAction<string | null>>;
  currentUser: User | null;
  currentContainer: Container | null;

  onPickStart: (bottle: InventoryBottleData, meta: PickMeta) => void;
}

const StationCtx = createContext<StationState | null>(null);

const SCAN_MS = 900;
const REJECT_HOLD_MS = 700;
const CONFETTI_MS = 800;
const POLL_MS = 5_000;
const MAX_WAIT_MS = 5 * 60_000;

interface PendingBottle {
  backendId: string;
  inventoryId: string;
  phase: 'inserted' | 'compacted';
  startedAt: number;
}

export function StationProvider({ children }: { children: ReactNode }) {
  const [inventory, setInventory] = useState<InventoryBottleData[]>(() => buildInventory(7, 20));

  const [dragging, setDragging] = useState<DragState | null>(null);
  const [dropArmed, setDropArmed] = useState(false);
  const [lidOpen, setLidOpen] = useState(false);
  const [scanning, setScanning] = useState(false);
  const [reject, setReject] = useState(false);

  const [crushed, setCrushed] = useState(0);
  const [fillPct, setFillPct] = useState(0);
  const [bottlesProcessed, setBottlesProcessed] = useState(0);

  const [users, setUsers] = useState<User[]>([]);
  const [containers, setContainers] = useState<Container[]>([]);
  const [currentUserId, setCurrentUserId] = useState<string | null>(null);
  const [currentContainerId, setCurrentContainerId] = useState<string | null>(null);

  const [activeStage, setActiveStage] = useState(-1);
  const [completed, setCompleted] = useState<Set<Stage>>(() => new Set());
  const [currentBottle, setCurrentBottle] = useState<InventoryBottleData | null>(null);
  const [aiResult, setAiResult] = useState<AiResult>(null);

  const [tokens, setTokens] = useState(0);
  const [bumpKey, setBumpKey] = useState(0);

  const [txLog, setTxLog] = useState<TxLogEntry[]>([]);

  const [confettiBursts, setConfettiBursts] = useState<ConfettiBurst[]>([]);

  const binRef = useRef<HTMLDivElement>(null);
  const walletRef = useRef<HTMLDivElement>(null);

  const draggingRef = useRef<DragState | null>(null);
  draggingRef.current = dragging;

  const handleDropRef = useRef<(b: InventoryBottleData) => void>(() => {});
  const pollRef = useRef<() => Promise<void>>(async () => {});
  const pendingBottleRef = useRef<PendingBottle | null>(null);
  const pollingTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);

  useEffect(() => () => {
    if (pollingTimerRef.current) {
      clearTimeout(pollingTimerRef.current);
      pollingTimerRef.current = null;
    }
  }, []);

  useEffect(() => {
    let alive = true;
    Promise.all([getUsers('recycler'), getContainers({ status: 'all' })])
      .then(([u, c]) => {
        if (!alive) return;
        setUsers(u);
        setContainers(c);
        if (u.length > 0) setCurrentUserId((prev) => prev ?? u[0].id);
        if (c.length > 0) setCurrentContainerId((prev) => prev ?? c[0].id);
      })
      .catch((err) => {
        toast.error(err instanceof Error ? err.message : 'Erro ao carregar dados da estação', { duration: 8000 });
      });
    return () => { alive = false; };
  }, []);

  const currentUser = useMemo(
    () => users.find((u) => u.id === currentUserId) ?? null,
    [users, currentUserId],
  );
  const currentContainer = useMemo(
    () => containers.find((c) => c.id === currentContainerId) ?? null,
    [containers, currentContainerId],
  );

  useEffect(() => {
    if (!currentContainer) return;
    const pct = currentContainer.capacity_liters > 0
      ? Math.round((currentContainer.current_volume_liters / currentContainer.capacity_liters) * 100)
      : 0;
    setFillPct(pct);
    setCrushed(0);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [currentContainerId]);

  useEffect(() => {
    if (!currentUserId) {
      setTokens(0);
      setTxLog([]);
      return;
    }
    let alive = true;
    getUserRewards(currentUserId)
      .then((data) => {
        if (!alive) return;
        setTokens(data.total_greentoken);
        setTxLog(data.rewards.map(rewardToTxLog));
        setBumpKey(0);
        setCompleted(new Set());
        setActiveStage(-1);
        setAiResult(null);
        setCurrentBottle(null);
      })
      .catch((err) => {
        if (!alive) return;
        toast.error(err instanceof Error ? err.message : 'Erro ao carregar recompensas', { duration: 8000 });
      });
    return () => { alive = false; };
  }, [currentUserId]);

  // --- Helpers para o pipeline ---

  const replaceInventoryBottle = useCallback((oldId: string) => {
    setInventory((list) => list.map((b) => (b.id === oldId ? makeReplacementBottle() : b)));
  }, []);

  const triggerConfetti = useCallback((count: number) => {
    if (!binRef.current || !walletRef.current) return;
    const bin = binRef.current.getBoundingClientRect();
    const wal = walletRef.current.getBoundingClientRect();
    const ox = bin.left + bin.width / 2;
    const oy = bin.top + bin.height * 0.4;
    const wx = wal.left + wal.width / 2;
    const wy = wal.top + wal.height / 2;
    const baseTx = wx - ox;
    const baseTy = wy - oy;
    const burst: ConfettiBurst[] = [];
    for (let i = 0; i < count; i++) {
      burst.push({
        id: `cb-${Date.now()}-${i}-${Math.random().toString(36).slice(2, 6)}`,
        ox,
        oy,
        tx: baseTx + (Math.random() - 0.5) * 60,
        ty: baseTy + (Math.random() - 0.5) * 60,
      });
    }
    setConfettiBursts((prev) => [...prev, ...burst]);
    const ids = new Set(burst.map((b) => b.id));
    window.setTimeout(() => {
      setConfettiBursts((prev) => prev.filter((b) => !ids.has(b.id)));
    }, CONFETTI_MS);
  }, []);

  // Refetch silencioso para reconciliar com o backend após cada confirmação.
  const refetchRewards = async () => {
    if (!currentUserId) return;
    try {
      const data = await getUserRewards(currentUserId);
      setTokens(data.total_greentoken);
      setTxLog(data.rewards.map(rewardToTxLog));
    } catch {
      // erro silencioso — próxima confirmação tenta de novo
    }
  };

  const refetchContainers = async () => {
    try {
      const data = await getContainers({ status: 'all' });
      setContainers(data);
    } catch {
      // erro silencioso
    }
  };

  // Polling de confirmação on-chain. Reatribuído a cada render para capturar
  // o estado fresco (currentContainer, currentUserId, etc.) sem reiniciar o timer.
  pollRef.current = async () => {
    const pending = pendingBottleRef.current;
    if (!pending) return;

    if (Date.now() - pending.startedAt > MAX_WAIT_MS) {
      pendingBottleRef.current = null;
      pollingTimerRef.current = null;
      toast.warning(
        'Confirmação on-chain está demorando demais — verifique em /dashboard/bottles.',
        { duration: 10000 },
      );
      setActiveStage(-1);
      replaceInventoryBottle(pending.inventoryId);
      return;
    }

    try {
      const remote = await getBottle(pending.backendId);

      if (pending.phase === 'inserted' && remote.utxo_hash) {
        const stage = STAGES[0];
        setCompleted((s) => {
          const n = new Set(s);
          n.add('inserted');
          return n;
        });
        setTokens((t) => t + stage.reward);
        setBumpKey((k) => k + 1);
        setBottlesProcessed((n) => n + 1);
        setTxLog((log) => [
          {
            id: `tx-opt-i-${remote.id}`,
            hash: remote.utxo_hash ?? '',
            label: stage.label,
            reward: stage.reward,
            datetime: fmtDateTime(new Date().toISOString()),
          },
          ...log,
        ]);
        triggerConfetti(2);
        setActiveStage(1);
        setLidOpen(false);
        setCrushed((c) => c + 1);
        void refetchRewards();
        void refetchContainers();
        pendingBottleRef.current = { ...pending, phase: 'compacted' };
      } else if (pending.phase === 'compacted' && remote.current_stage === 'compacted') {
        const stage = STAGES[1];
        setCompleted((s) => {
          const n = new Set(s);
          n.add('compacted');
          return n;
        });
        setTokens((t) => t + stage.reward);
        setBumpKey((k) => k + 1);
        setTxLog((log) => [
          {
            id: `tx-opt-c-${remote.id}`,
            hash: remote.utxo_hash ?? '',
            label: stage.label,
            reward: stage.reward,
            datetime: fmtDateTime(new Date().toISOString()),
          },
          ...log,
        ]);
        triggerConfetti(3);
        setActiveStage(-1);
        replaceInventoryBottle(pending.inventoryId);
        void refetchRewards();
        void refetchContainers();
        pendingBottleRef.current = null;
        pollingTimerRef.current = null;
        return;
      }
    } catch {
      // erro de rede transiente — apenas reagenda
    }

    pollingTimerRef.current = window.setTimeout(() => pollRef.current(), POLL_MS);
  };

  // Atualizado a cada render para capturar estado fresco; a referência estável é
  // exposta via handleDropRef para os listeners de documento.
  handleDropRef.current = (b: InventoryBottleData) => {
    if (!currentUser || !currentContainer) {
      toast.error('Selecione um reciclador e um container válidos.');
      return;
    }
    if (currentContainer.status !== 'active') {
      toast.error('Container indisponível para inserções no momento.');
      return;
    }

    setCurrentBottle(b);
    setCompleted(new Set());
    setActiveStage(-1);
    setAiResult('validating');
    setLidOpen(true);
    setScanning(true);

    // Garrafa inválida (can/glass): reject puramente client-side, sem API.
    if (b.invalid) {
      window.setTimeout(() => {
        setScanning(false);
        setAiResult('rejected');
        setReject(true);
        setLidOpen(false);
        window.setTimeout(() => {
          setReject(false);
          setActiveStage(-1);
          setAiResult(null);
          replaceInventoryBottle(b.id);
        }, REJECT_HOLD_MS);
      }, SCAN_MS);
      return;
    }

    const userId = currentUser.id;
    const containerId = currentContainer.id;
    const cap = currentContainer.capacity_liters;
    const liters = volumeMlOf(b.size) / 1000;

    const start = Date.now();
    window.setTimeout(() => setScanning(false), SCAN_MS);

    const waitScan = async () => {
      const elapsed = Date.now() - start;
      if (elapsed < SCAN_MS) {
        await new Promise((r) => window.setTimeout(r, SCAN_MS - elapsed));
      }
    };

    createBottle({
      user_id: userId,
      container_id: containerId,
      volume_ml: volumeMlOf(b.size),
    })
      .then(async (result) => {
        await waitScan();
        setScanning(false);
        setAiResult('accepted');
        setActiveStage(0);
        toast.success(
          `Garrafa "${result.bottle.bottle_id_text}" criada. Aguardando confirmação on-chain…`,
          {
            description: result.tx_hash ? `tx: ${result.tx_hash.slice(0, 16)}…` : undefined,
            duration: 5000,
          },
        );

        // Atualização otimista do volume — backend já contabilizou ao criar.
        if (cap > 0) {
          setContainers((list) =>
            list.map((c) =>
              c.id === containerId
                ? {
                    ...c,
                    current_volume_liters: Math.min(
                      cap,
                      c.current_volume_liters + liters,
                    ),
                  }
                : c,
            ),
          );
          setFillPct((prev) => Math.min(100, Math.round(prev + (liters / cap) * 100)));
        }
        void refetchContainers();

        pendingBottleRef.current = {
          backendId: result.bottle.id,
          inventoryId: b.id,
          phase: 'inserted',
          startedAt: Date.now(),
        };
        if (pollingTimerRef.current) clearTimeout(pollingTimerRef.current);
        pollingTimerRef.current = window.setTimeout(() => pollRef.current(), POLL_MS);
      })
      .catch(async (err) => {
        await waitScan();
        setScanning(false);
        setAiResult('rejected');
        setReject(true);
        setLidOpen(false);
        const msg = err instanceof Error ? err.message : 'Erro ao criar garrafa';
        toast.error(msg, { duration: 10000 });
        window.setTimeout(() => {
          setReject(false);
          setActiveStage(-1);
          setAiResult(null);
          replaceInventoryBottle(b.id);
        }, REJECT_HOLD_MS);
      });
  };

  const onPickStart = useCallback(
    (b: InventoryBottleData, meta: PickMeta) => {
      if (activeStage >= 0 || draggingRef.current) return;
      document.body.classList.add('gt-dragging');
      setDragging({
        bottle: b,
        offsetX: meta.offsetX,
        offsetY: meta.offsetY,
        x: meta.startX + meta.offsetX,
        y: meta.startY + meta.offsetY,
      });
      setDropArmed(false);
    },
    [activeStage],
  );

  // Listeners globais para arrastar a garrafa (mount-once).
  useEffect(() => {
    const getPoint = (e: MouseEvent | TouchEvent): { x: number; y: number } | null => {
      if ('touches' in e) {
        const te = e as TouchEvent;
        const t = te.touches[0] ?? te.changedTouches[0];
        return t ? { x: t.clientX, y: t.clientY } : null;
      }
      const m = e as MouseEvent;
      return { x: m.clientX, y: m.clientY };
    };

    const isInsideBin = (x: number, y: number) => {
      if (!binRef.current) return false;
      const r = binRef.current.getBoundingClientRect();
      return x >= r.left && x <= r.right && y >= r.top && y <= r.bottom;
    };

    const onMove = (e: MouseEvent | TouchEvent) => {
      if (!draggingRef.current) return;
      if ('touches' in e) e.preventDefault();
      const p = getPoint(e);
      if (!p) return;
      setDragging((prev) => (prev ? { ...prev, x: p.x, y: p.y } : null));
      setDropArmed(isInsideBin(p.x, p.y));
    };

    const onUp = (e: MouseEvent | TouchEvent) => {
      const cur = draggingRef.current;
      if (!cur) return;
      const p = getPoint(e);
      const inside = !!p && isInsideBin(p.x, p.y);
      document.body.classList.remove('gt-dragging');
      setDragging(null);
      setDropArmed(false);
      if (inside) handleDropRef.current(cur.bottle);
    };

    document.addEventListener('mousemove', onMove);
    document.addEventListener('mouseup', onUp);
    document.addEventListener('touchmove', onMove, { passive: false });
    document.addEventListener('touchend', onUp);
    return () => {
      document.removeEventListener('mousemove', onMove);
      document.removeEventListener('mouseup', onUp);
      document.removeEventListener('touchmove', onMove);
      document.removeEventListener('touchend', onUp);
    };
  }, []);

  const value = useMemo<StationState>(
    () => ({
      inventory, setInventory,
      dragging, setDragging,
      dropArmed, setDropArmed,
      lidOpen, setLidOpen,
      scanning, setScanning,
      reject, setReject,
      crushed, setCrushed,
      fillPct, setFillPct,
      bottlesProcessed, setBottlesProcessed,
      activeStage, setActiveStage,
      completed, setCompleted,
      currentBottle, setCurrentBottle,
      aiResult, setAiResult,
      tokens, setTokens,
      bumpKey, setBumpKey,
      txLog, setTxLog,
      binRef, walletRef,
      users,
      containers, setContainers,
      currentUserId, setCurrentUserId,
      currentContainerId, setCurrentContainerId,
      currentUser,
      currentContainer,
      onPickStart,
    }),
    [
      inventory, dragging, dropArmed, lidOpen, scanning, reject,
      crushed, fillPct, bottlesProcessed,
      activeStage, completed, currentBottle, aiResult,
      tokens, bumpKey, txLog,
      users, containers, currentUserId, currentContainerId, currentUser, currentContainer,
      onPickStart,
    ],
  );

  return (
    <StationCtx.Provider value={value}>
      {children}
      {dragging && (
        <div
          className="pointer-events-none fixed z-[80]"
          style={{
            left: dragging.x - dragging.offsetX,
            top: dragging.y - dragging.offsetY,
            transform: dropArmed ? 'scale(1.06)' : 'scale(1)',
            transition: 'transform 180ms ease',
            filter: dropArmed
              ? 'drop-shadow(0 8px 16px rgba(22,163,74,0.45))'
              : 'drop-shadow(0 4px 12px rgba(0,0,0,0.18))',
          }}
        >
          <Bottle
            size={dragging.bottle.size}
            tint={dragging.bottle.tint}
            kind={dragging.bottle.kind}
            invalid={dragging.bottle.invalid}
          />
        </div>
      )}
      {confettiBursts.map((b) => (
        <span
          key={b.id}
          className="pointer-events-none fixed z-[70] animate-gt-coin-fly text-gt-600 font-bold text-xl"
          style={{
            left: b.ox,
            top: b.oy,
            ['--tx' as string]: `${b.tx}px`,
            ['--ty' as string]: `${b.ty}px`,
          }}
        >
          ₲
        </span>
      ))}
    </StationCtx.Provider>
  );
}

export function useStation(): StationState {
  const ctx = useContext(StationCtx);
  if (!ctx) throw new Error('useStation must be used inside <StationProvider>');
  return ctx;
}
