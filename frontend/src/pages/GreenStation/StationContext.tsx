import {
  createContext,
  useContext,
  useMemo,
  useRef,
  useState,
  type ReactNode,
  type RefObject,
} from 'react';
import type {
  AiResult,
  DragState,
  InventoryBottleData,
  Stage,
  TxLogEntry,
} from './types';
import { buildInventory, fakeTxHash } from './helpers';

interface ConfettiBurst {
  id: string;
  ox: number;
  oy: number;
  tx: number;
  ty: number;
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

  confettiBursts: ConfettiBurst[];
  setConfettiBursts: React.Dispatch<React.SetStateAction<ConfettiBurst[]>>;

  binRef: RefObject<HTMLDivElement>;
  walletRef: RefObject<HTMLDivElement>;

  // Etapa 2+: handlers reais. Por enquanto são no-op.
  onPickStart: (bottle: InventoryBottleData, meta: { offsetX: number; offsetY: number; startX: number; startY: number }) => void;
  handleDrop: (bottle: InventoryBottleData) => void;
}

const StationCtx = createContext<StationState | null>(null);

export function StationProvider({ children }: { children: ReactNode }) {
  const [inventory, setInventory] = useState<InventoryBottleData[]>(() => buildInventory(7, 10));

  const [dragging, setDragging] = useState<DragState | null>(null);
  const [dropArmed, setDropArmed] = useState(false);
  const [lidOpen, setLidOpen] = useState(false);
  const [scanning, setScanning] = useState(false);
  const [reject, setReject] = useState(false);

  const [crushed, setCrushed] = useState(0);
  const [fillPct, setFillPct] = useState(89);
  const [bottlesProcessed, setBottlesProcessed] = useState(8);

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
      confettiBursts, setConfettiBursts,
      binRef, walletRef,
      onPickStart: () => {},
      handleDrop: () => {},
    }),
    [
      inventory, dragging, dropArmed, lidOpen, scanning, reject,
      crushed, fillPct, bottlesProcessed,
      activeStage, completed, currentBottle, aiResult,
      tokens, bumpKey, txLog, confettiBursts,
    ],
  );

  return <StationCtx.Provider value={value}>{children}</StationCtx.Provider>;
}

export function useStation(): StationState {
  const ctx = useContext(StationCtx);
  if (!ctx) throw new Error('useStation must be used inside <StationProvider>');
  return ctx;
}
