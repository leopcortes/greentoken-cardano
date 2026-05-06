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
  fakeTxHash,
  makeReplacementBottle,
  volumeMlOf,
} from './helpers';
import { Bottle } from '@/components/Bottle';
import { getContainers, getUsers, type Container, type User } from '@/services/api';

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

const fmtNow = () => {
  const d = new Date();
  const p = (n: number) => String(n).padStart(2, '0');
  return `${p(d.getDate())}/${p(d.getMonth() + 1)}/${String(d.getFullYear()).slice(-2)} ${p(d.getHours())}:${p(d.getMinutes())}`;
};

const SCAN_MS = 900;
const REJECT_HOLD_MS = 700;
const STAGE_INSERTED_MS = 700;
const STAGE_COMPACTED_MS = 900;
const CONFETTI_MS = 800;

export function StationProvider({ children }: { children: ReactNode }) {
  const [inventory, setInventory] = useState<InventoryBottleData[]>(() => buildInventory(7, 20));

  const [dragging, setDragging] = useState<DragState | null>(null);
  const [dropArmed, setDropArmed] = useState(false);
  const [lidOpen, setLidOpen] = useState(false);
  const [scanning, setScanning] = useState(false);
  const [reject, setReject] = useState(false);

  const [crushed, setCrushed] = useState(0);
  const [fillPct, setFillPct] = useState(0);
  const [bottlesProcessed, setBottlesProcessed] = useState(8);

  const [users, setUsers] = useState<User[]>([]);
  const [containers, setContainers] = useState<Container[]>([]);
  const [currentUserId, setCurrentUserId] = useState<string | null>(null);
  const [currentContainerId, setCurrentContainerId] = useState<string | null>(null);

  const [activeStage, setActiveStage] = useState(-1);
  const [completed, setCompleted] = useState<Set<Stage>>(() => new Set());
  const [currentBottle, setCurrentBottle] = useState<InventoryBottleData | null>(null);
  const [aiResult, setAiResult] = useState<AiResult>(null);

  const [tokens, setTokens] = useState(127);
  const [bumpKey, setBumpKey] = useState(0);

  const [txLog, setTxLog] = useState<TxLogEntry[]>(() => {
    const seed = [
      { label: 'Compactada', reward: 3,  datetime: '05/05/26 14:02' },
      { label: 'Inserida',   reward: 10, datetime: '05/05/26 13:58' },
      { label: 'Compactada', reward: 3,  datetime: '05/05/26 13:41' },
      { label: 'Inserida',   reward: 10, datetime: '05/05/26 13:29' },
      { label: 'Inserida',   reward: 10, datetime: '05/05/26 13:12' },
      { label: 'Compactada', reward: 3,  datetime: '04/05/26 12:55' },
      { label: 'Inserida',   reward: 10, datetime: '04/05/26 12:34' },
      { label: 'Compactada', reward: 3,  datetime: '04/05/26 12:18' },
      { label: 'Inserida',   reward: 10, datetime: '04/05/26 11:47' },
      { label: 'Compactada', reward: 3,  datetime: '04/05/26 11:21' },
    ];
    return seed.map((s, i) => ({ id: `init-${i}`, hash: fakeTxHash(), ...s }));
  });

  const [confettiBursts, setConfettiBursts] = useState<ConfettiBurst[]>([]);

  const binRef = useRef<HTMLDivElement>(null);
  const walletRef = useRef<HTMLDivElement>(null);

  const draggingRef = useRef<DragState | null>(null);
  draggingRef.current = dragging;

  const handleDropRef = useRef<(b: InventoryBottleData) => void>(() => {});

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

  // Atualizado a cada render para capturar estado fresco; a referência estável é
  // exposta via handleDropRef para os listeners de documento.
  handleDropRef.current = (b: InventoryBottleData) => {
    setCurrentBottle(b);
    setCompleted(new Set());
    setActiveStage(-1);
    setAiResult('validating');
    setLidOpen(true);
    setScanning(true);

    window.setTimeout(() => {
      setScanning(false);

      if (b.invalid) {
        setAiResult('rejected');
        setReject(true);
        setLidOpen(false);
        window.setTimeout(() => {
          setReject(false);
          replaceInventoryBottle(b.id);
        }, REJECT_HOLD_MS);
        return;
      }

      setAiResult('accepted');
      setActiveStage(0);

      window.setTimeout(() => {
        const insertedStage = STAGES[0];
        setCompleted((prev) => {
          const next = new Set(prev);
          next.add('inserted');
          return next;
        });
        setTokens((t) => t + insertedStage.reward);
        setBumpKey((k) => k + 1);
        setBottlesProcessed((n) => n + 1);
        setTxLog((log) => [
          {
            id: `tx-${Date.now()}-i`,
            hash: fakeTxHash(),
            label: insertedStage.label,
            reward: insertedStage.reward,
            datetime: fmtNow(),
          },
          ...log,
        ]);
        triggerConfetti(2);

        setActiveStage(1);
        setLidOpen(false);
        setCrushed((c) => c + 1);

        const cont = currentContainer;
        if (cont && cont.capacity_liters > 0) {
          const liters = volumeMlOf(b.size) / 1000;
          setContainers((list) =>
            list.map((c) =>
              c.id === cont.id
                ? {
                    ...c,
                    current_volume_liters: Math.min(
                      c.capacity_liters,
                      c.current_volume_liters + liters,
                    ),
                  }
                : c,
            ),
          );
          setFillPct((prev) =>
            Math.min(100, Math.round(prev + (liters / cont.capacity_liters) * 100)),
          );
        }

        window.setTimeout(() => {
          const compactedStage = STAGES[1];
          setCompleted((prev) => {
            const next = new Set(prev);
            next.add('compacted');
            return next;
          });
          setTokens((t) => t + compactedStage.reward);
          setBumpKey((k) => k + 1);
          setTxLog((log) => [
            {
              id: `tx-${Date.now()}-c`,
              hash: fakeTxHash(),
              label: compactedStage.label,
              reward: compactedStage.reward,
              datetime: fmtNow(),
            },
            ...log,
          ]);
          triggerConfetti(3);
          setActiveStage(-1);
          replaceInventoryBottle(b.id);
        }, STAGE_COMPACTED_MS);
      }, STAGE_INSERTED_MS);
    }, SCAN_MS);
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
