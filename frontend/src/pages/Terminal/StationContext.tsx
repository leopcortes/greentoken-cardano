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
} from '../../lib/types';
import {
  STAGES,
  buildInventory,
  fmtDateTime,
  makeInvalidReplacement,
  makeReplacementBottle,
  rewardToTxLog,
  volumeMlOf,
} from '@/lib/helpers';
import { Bottle } from '@/components/Bottle';
import { playBottleDrop, playCoinReward } from '@/lib/sounds';
import {
  createBottle,
  getBottle,
  getContainers,
  getGreenwalletBalance,
  getUser,
  getUserRewards,
  type Bottle as ApiBottle,
  type Container,
  type Reward,
  type User,
} from '@/services/api';
import { useAuth } from '@/auth/AuthContext';
import { useIdleTimeout } from '@/hooks/useIdleTimeout';
import { useNavigate } from 'react-router-dom';

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
  ada: string | null;
  bumpKey: number;
  setBumpKey: React.Dispatch<React.SetStateAction<number>>;

  txLog: TxLogEntry[];
  setTxLog: React.Dispatch<React.SetStateAction<TxLogEntry[]>>;

  binRef: RefObject<HTMLDivElement>;
  walletRef: RefObject<HTMLDivElement>;

  containers: Container[];
  setContainers: React.Dispatch<React.SetStateAction<Container[]>>;
  currentUserId: string | null;
  currentContainerId: string | null;
  setCurrentContainerId: React.Dispatch<React.SetStateAction<string | null>>;
  currentUser: User | null;
  currentContainer: Container | null;

  currentBottleApi: ApiBottle | null;

  processingStartedAt: number | null;

  inFlight: InFlightBottle[];

  onPickStart: (bottle: InventoryBottleData, meta: PickMeta) => void;
}

export interface InFlightBottle {
  backendId: string;
  bottleIdText: string;
  inventoryId: string;
  phase: 'inserted' | 'compacted';
  enqueuedAt: number;
  compactingAt: number | null;
  insertedTxHash: string | null;
  compactedTxHash: string | null;
}

const StationCtx = createContext<StationState | null>(null);

const SCAN_MS = 900;
const REJECT_HOLD_MS = 700;
const CONFETTI_MS = 800;
const POLL_MS = 2_000;
const MAX_WAIT_MS = 5 * 60_000;

// Erros do backend chegam no formato `"<status>: <body>"` (api.ts), e o body
// frequentemente é `{"error":"..."}`. Extrai a mensagem legível para o toast.
function humanizeApiError(err: unknown, fallback: string): string {
  const raw = err instanceof Error ? err.message : String(err ?? fallback);
  const match = raw.match(/^\d+:\s*(.+)$/s);
  const body = match ? match[1] : raw;
  try {
    const parsed = JSON.parse(body);
    if (parsed && typeof parsed === 'object' && typeof parsed.error === 'string') {
      return parsed.error;
    }
  } catch {
    // body não é JSON - usa como está
  }
  return body || fallback;
}

export function StationProvider({ children }: { children: ReactNode }) {
  const [inventory, setInventory] = useState<InventoryBottleData[]>(() => buildInventory(7, 12));

  const [dragging, setDragging] = useState<DragState | null>(null);
  const [dropArmed, setDropArmed] = useState(false);
  const [lidOpen, setLidOpen] = useState(false);
  const [scanning, setScanning] = useState(false);
  const [reject, setReject] = useState(false);

  const [crushed, setCrushed] = useState(0);
  const [fillPct, setFillPct] = useState(0);
  const [rewards, setRewards] = useState<Reward[]>([]);

  // Conta garrafas inseridas hoje (dia local) com base nas recompensas de
  // stage='inserted', para refletir o dia inteiro e não apenas a sessão atual.
  const bottlesProcessed = useMemo(() => {
    const now = new Date();
    const y = now.getFullYear();
    const m = now.getMonth();
    const d = now.getDate();
    return rewards.reduce((acc, r) => {
      if (r.stage !== 'inserted') return acc;
      const t = new Date(r.sent_at);
      if (Number.isNaN(t.getTime())) return acc;
      if (t.getFullYear() === y && t.getMonth() === m && t.getDate() === d) return acc + 1;
      return acc;
    }, 0);
  }, [rewards]);

  const { user: authUser, logout } = useAuth();
  const navigate = useNavigate();
  const currentUserId = authUser?.role === 'recycler' ? authUser.userId : null;

  const [currentUser, setCurrentUser] = useState<User | null>(null);
  const [containers, setContainers] = useState<Container[]>([]);
  const [currentContainerId, setCurrentContainerId] = useState<string | null>(null);

  const [activeStage, setActiveStage] = useState(-1);
  const [completed, setCompleted] = useState<Set<Stage>>(() => new Set());
  const [currentBottle, setCurrentBottle] = useState<InventoryBottleData | null>(null);
  const [currentBottleApi, setCurrentBottleApi] = useState<ApiBottle | null>(null);
  const [aiResult, setAiResult] = useState<AiResult>(null);

  const [tokens, setTokens] = useState(0);
  const [ada, setAda] = useState<string | null>(null);
  const [bumpKey, setBumpKey] = useState(0);

  const [processingStartedAt, setProcessingStartedAt] = useState<number | null>(null);

  const [txLog, setTxLog] = useState<TxLogEntry[]>([]);

  const [confettiBursts, setConfettiBursts] = useState<ConfettiBurst[]>([]);

  const binRef = useRef<HTMLDivElement>(null);
  const walletRef = useRef<HTMLDivElement>(null);

  const draggingRef = useRef<DragState | null>(null);
  draggingRef.current = dragging;

  const handleDropRef = useRef<(b: InventoryBottleData) => void>(() => {});
  const pollRef = useRef<() => Promise<void>>(async () => {});
  const [inFlight, setInFlight] = useState<InFlightBottle[]>([]);
  const inFlightRef = useRef<InFlightBottle[]>([]);
  inFlightRef.current = inFlight;
  const pollingTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);

  const schedulePoll = useCallback(() => {
    if (pollingTimerRef.current) return;
    pollingTimerRef.current = window.setTimeout(() => {
      pollingTimerRef.current = null;
      pollRef.current();
    }, POLL_MS);
  }, []);

  useEffect(() => () => {
    if (pollingTimerRef.current) {
      clearTimeout(pollingTimerRef.current);
      pollingTimerRef.current = null;
    }
  }, []);

  // Carrega o perfil do reciclador logado (vindo do AuthContext) e a lista de
  // containers. O usuario nao e' mais selecionavel - vem do token.
  useEffect(() => {
    let alive = true;
    if (!currentUserId) {
      setCurrentUser(null);
      return;
    }
    Promise.all([getUser(currentUserId), getContainers({ status: 'all' })])
      .then(([u, c]) => {
        if (!alive) return;
        setCurrentUser(u);
        setContainers(c);
        if (c.length > 0) setCurrentContainerId((prev) => prev ?? c[0].id);
      })
      .catch((err) => {
        toast.error(humanizeApiError(err, 'Erro ao carregar dados da estação'), { duration: 10000 });
      });
    return () => { alive = false; };
  }, [currentUserId]);

  const currentContainer = useMemo(
    () => containers.find((c) => c.id === currentContainerId) ?? null,
    [containers, currentContainerId],
  );

  // Timeout de inatividade do terminal: ao expirar, desloga o reciclador e volta
  // para a tela idle. Reset disparado por interacoes explicitas (drop, pick,
  // troca de container) - ver chamadas a idle.reset() abaixo.
  const TERMINAL_IDLE_MS = 3 * 60_000;
  const idle = useIdleTimeout(
    TERMINAL_IDLE_MS,
    () => {
      toast.info('Sessão expirada por inatividade. Voltando para a tela inicial.', {
        duration: 6000,
      });
      logout();
      navigate('/', { replace: true });
    },
    !!currentUserId,
  );

  // Reset visual ao trocar de container (zera empilhamento de garrafas compactadas).
  useEffect(() => {
    setCrushed(0);
  }, [currentContainerId]);

  // Mantém o fillPct em sincronia com o estado real do container - dispara
  // tanto na seleção inicial quanto quando refetchContainers atualiza o volume.
  useEffect(() => {
    if (!currentContainer) return;
    const pct = currentContainer.capacity_liters > 0
      ? Math.round((currentContainer.current_volume_liters / currentContainer.capacity_liters) * 100)
      : 0;
    setFillPct(pct);
  }, [currentContainer?.id, currentContainer?.current_volume_liters, currentContainer?.capacity_liters]);

  useEffect(() => {
    if (!currentUserId) {
      setTokens(0);
      setAda(null);
      setTxLog([]);
      setRewards([]);
      return;
    }
    let alive = true;
    Promise.all([
      getUserRewards(currentUserId),
      // Saldo on-chain via Blockfrost - fonte da verdade para tokens/ADA.
      // Falha silenciosa cai para o saldo do DB (recompensas) abaixo.
      getGreenwalletBalance(currentUserId).catch(() => null),
    ])
      .then(([rewardsData, balance]) => {
        if (!alive) return;
        setTokens(balance ? balance.greentoken : rewardsData.total_greentoken);
        setAda(balance ? balance.ada : null);
        setTxLog(rewardsData.rewards.map(rewardToTxLog));
        setRewards(rewardsData.rewards);
        setBumpKey(0);
        setCompleted(new Set());
        setActiveStage(-1);
        setAiResult(null);
        setCurrentBottle(null);
        setCurrentBottleApi(null);
      })
      .catch((err) => {
        if (!alive) return;
        toast.error(humanizeApiError(err, 'Erro ao carregar recompensas'), { duration: 10000 });
      });
    return () => { alive = false; };
  }, [currentUserId]);

  // --- Helpers para o pipeline ---

  const replaceInventoryBottle = useCallback((oldId: string) => {
    setInventory((list) => {
      const remaining = list.filter((b) => b.id !== oldId);
      const hasCan = remaining.some((b) => b.invalid === 'can');
      const hasGlass = remaining.some((b) => b.invalid === 'glass');
      const replacement = !hasCan
        ? makeInvalidReplacement('can')
        : !hasGlass
          ? makeInvalidReplacement('glass')
          : makeReplacementBottle();
      return list.map((b) => (b.id === oldId ? replacement : b));
    });
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
  // Tokens/ADA vem do saldo on-chain (Blockfrost); txLog vem do DB de rewards.
  const refetchRewards = async () => {
    if (!currentUserId) return;
    try {
      const [rewardsData, balance] = await Promise.all([
        getUserRewards(currentUserId),
        getGreenwalletBalance(currentUserId).catch(() => null),
      ]);
      setTokens(balance ? balance.greentoken : rewardsData.total_greentoken);
      setAda(balance ? balance.ada : null);
      setTxLog(rewardsData.rewards.map(rewardToTxLog));
      setRewards(rewardsData.rewards);
    } catch {
      // erro silencioso - próxima confirmação tenta de novo
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

  // Polling paralelo: a cada ciclo, busca todas as garrafas em voo de uma vez
  // (Promise.allSettled) e processa cada confirmação independente. Reatribuído
  // a cada render para capturar estado fresco sem reiniciar o timer.
  pollRef.current = async () => {
    const list = inFlightRef.current;
    if (list.length === 0) return;

    const results = await Promise.allSettled(list.map((b) => getBottle(b.backendId)));

    let rewardsTouched = false;
    let containersTouched = false;
    const next: InFlightBottle[] = [];

    for (let i = 0; i < list.length; i++) {
      const bottle = list[i];
      const result = results[i];

      // Timeout por fase: 5 min desde enqueue (inserted) ou desde compactingAt (compacted).
      const phaseStart = bottle.phase === 'compacted' && bottle.compactingAt != null
        ? bottle.compactingAt
        : bottle.enqueuedAt;
      const timedOut = Date.now() - phaseStart > MAX_WAIT_MS;

      if (result.status !== 'fulfilled') {
        // Erro de rede transiente: mantém, retenta no próximo ciclo (a menos que tenha esgotado).
        if (timedOut) {
          toast.warning(
            `Confirmação de ${bottle.bottleIdText} demorando demais — verifique em /dashboard/bottles.`,
            { duration: 12000 },
          );
          replaceInventoryBottle(bottle.inventoryId);
          if (currentBottleApi?.id === bottle.backendId) {
            setActiveStage(-1);
            setProcessingStartedAt(null);
          }
          continue;
        }
        next.push(bottle);
        continue;
      }

      const { bottle: remote, txs } = result.value;
      const insertedTx = txs.find((t) => t.stage === 'inserted' && t.status === 'confirmed');
      const compactedTx = txs.find((t) => t.stage === 'compacted' && t.status === 'confirmed');
      const isCurrent = currentBottleApi?.id === bottle.backendId;

      let phase = bottle.phase;
      let compactingAt = bottle.compactingAt;
      let insertedTxHash = bottle.insertedTxHash;
      let compactedTxHash = bottle.compactedTxHash;

      if (phase === 'inserted' && insertedTx) {
        const stage = STAGES[0];
        setTokens((t) => t + stage.reward);
        setBumpKey((k) => k + 1);
        setTxLog((log) => [
          {
            id: `tx-opt-i-${remote.id}`,
            hash: insertedTx.tx_hash ?? '',
            label: stage.label,
            reward: stage.reward,
            datetime: fmtDateTime(new Date().toISOString()),
          },
          ...log,
        ]);
        triggerConfetti(2);
        playCoinReward();
        if (isCurrent) {
          setCompleted((s) => {
            const n = new Set(s);
            n.add('inserted');
            return n;
          });
          setActiveStage(1);
          setLidOpen(false);
        }
        setCrushed((c) => c + 1);
        toast.success(
          `${bottle.bottleIdText}: inserção confirmada — compactando…`,
          { duration: 3000 },
        );
        rewardsTouched = true;
        containersTouched = true;
        phase = 'compacted';
        compactingAt = Date.now();
        insertedTxHash = insertedTx.tx_hash ?? null;
      }

      // Sem `else`: se ambas confirmações estão prontas no mesmo poll, processa as duas.
      if (phase === 'compacted' && compactedTx) {
        const stage = STAGES[1];
        setTokens((t) => t + stage.reward);
        setBumpKey((k) => k + 1);
        setTxLog((log) => [
          {
            id: `tx-opt-c-${remote.id}`,
            hash: compactedTx.tx_hash ?? '',
            label: stage.label,
            reward: stage.reward,
            datetime: fmtDateTime(new Date().toISOString()),
          },
          ...log,
        ]);
        triggerConfetti(3);
        playCoinReward();
        if (isCurrent) {
          setCompleted((s) => {
            const n = new Set(s);
            n.add('compacted');
            return n;
          });
          setActiveStage(-1);
          setProcessingStartedAt(null);
        }
        rewardsTouched = true;
        containersTouched = true;
        compactedTxHash = compactedTx.tx_hash ?? null;
        // garrafa finalizou o pipeline — não re-adicionar a `next`.
        continue;
      }

      if (timedOut) {
        toast.warning(
          `Compactação de ${bottle.bottleIdText} demorando demais — verifique em /dashboard/bottles.`,
          { duration: 12000 },
        );
        replaceInventoryBottle(bottle.inventoryId);
        if (isCurrent) {
          setActiveStage(-1);
          setProcessingStartedAt(null);
        }
        continue;
      }

      next.push({ ...bottle, phase, compactingAt, insertedTxHash, compactedTxHash });
    }

    setInFlight(next);

    if (rewardsTouched) void refetchRewards();
    if (containersTouched) void refetchContainers();

    if (next.length > 0) schedulePoll();
  };

  // Atualizado a cada render para capturar estado fresco; a referência estável é
  // exposta via handleDropRef para os listeners de documento.
  handleDropRef.current = (b: InventoryBottleData) => {
    if (!currentUser || !currentContainer) {
      toast.error('Selecione um reciclador e um container válidos.', { duration: 8000 });
      return;
    }
    if (currentContainer.status !== 'active') {
      toast.error('Container indisponível para inserções no momento.', { duration: 8000 });
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
      const material = b.invalid === 'can' ? 'Lata de alumínio' : 'Vidro';
      window.setTimeout(() => {
        setScanning(false);
        setAiResult('rejected');
        setReject(true);
        setLidOpen(false);
        toast.error(`A IA do container rejeitou o item ${material}.`, {
          description: 'Apenas garrafas PET/HDPE são aceitas.',
          duration: 6000,
        });
        window.setTimeout(() => {
          setReject(false);
          setActiveStage(-1);
          setAiResult(null);
          replaceInventoryBottle(b.id);
        }, REJECT_HOLD_MS);
      }, SCAN_MS);
      return;
    }

    setCurrentBottleApi(null);
    const containerId = currentContainer.id;

    const start = Date.now();
    setProcessingStartedAt(start);
    window.setTimeout(() => setScanning(false), SCAN_MS);

    const waitScan = async () => {
      const elapsed = Date.now() - start;
      if (elapsed < SCAN_MS) {
        await new Promise((r) => window.setTimeout(r, SCAN_MS - elapsed));
      }
    };

    idle.reset();
    createBottle({
      container_id: containerId,
      volume_ml: volumeMlOf(b.size),
    })
      .then(async (result) => {
        await waitScan();
        setScanning(false);
        setAiResult('accepted');
        setActiveStage(0);
        setCurrentBottleApi(result.bottle);
        window.setTimeout(playBottleDrop, 500);
        toast.success(
          `Garrafa "${result.bottle.bottle_id_text}" validada pela IA e inserida no container.`,
          {
            description: result.tx_hash
              ? `Aguardando confirmação on-chain - tx: ${result.tx_hash.slice(0, 16)}…`
              : 'Aguardando confirmação on-chain…',
            duration: 8000,
          },
        );

        // Enfileira no pipeline: a partir daqui o usuário pode arrastar outra
        // garrafa enquanto esta confirma e compacta em background.
        setInFlight((list) => [
          ...list,
          {
            backendId: result.bottle.id,
            bottleIdText: result.bottle.bottle_id_text,
            inventoryId: b.id,
            phase: 'inserted',
            enqueuedAt: Date.now(),
            compactingAt: null,
            insertedTxHash: result.tx_hash ?? null,
            compactedTxHash: null,
          },
        ]);
        // Slot do inventário libera imediatamente — a garrafa já está "no
        // container" do ponto de vista do usuário.
        replaceInventoryBottle(b.id);
        schedulePoll();
      })
      .catch(async (err) => {
        await waitScan();
        setScanning(false);
        setAiResult('rejected');
        setReject(true);
        setLidOpen(false);
        setProcessingStartedAt(null);
        toast.error(humanizeApiError(err, 'Erro ao criar garrafa'), { duration: 12000 });
        window.setTimeout(() => {
          setReject(false);
          setActiveStage(-1);
          setAiResult(null);
          replaceInventoryBottle(b.id);
        }, REJECT_HOLD_MS);
      });
  };

  const scanningRef = useRef(false);
  scanningRef.current = scanning;
  const onPickStart = useCallback(
    (b: InventoryBottleData, meta: PickMeta) => {
      if (scanningRef.current || draggingRef.current) return;
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
    [],
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
      bottlesProcessed,
      activeStage, setActiveStage,
      completed, setCompleted,
      currentBottle, setCurrentBottle,
      aiResult, setAiResult,
      tokens, setTokens,
      ada,
      bumpKey, setBumpKey,
      txLog, setTxLog,
      binRef, walletRef,
      containers, setContainers,
      currentUserId,
      currentContainerId, setCurrentContainerId,
      currentUser,
      currentContainer,
      currentBottleApi,
      processingStartedAt,
      inFlight,
      onPickStart,
    }),
    [
      inventory, dragging, dropArmed, lidOpen, scanning, reject,
      crushed, fillPct, bottlesProcessed,
      activeStage, completed, currentBottle, currentBottleApi, aiResult,
      tokens, ada, bumpKey, txLog,
      containers, currentUserId, currentContainerId, currentUser, currentContainer,
      processingStartedAt,
      inFlight,
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
