export type BottleSize = 'S' | 'M' | 'L';
export type BottleTint = 'clear' | 'green' | 'blue' | 'amber';
export type BottleKind = 'PET' | 'HDPE' | 'AL' | 'GLASS';
export type BottleInvalid = 'can' | 'glass' | null;

export interface InventoryBottleData {
  id: string;
  size: BottleSize;
  tint: BottleTint;
  kind: BottleKind;
  invalid: BottleInvalid;
  rot: string;
}

export type Stage = 'inserted' | 'compacted';

export interface StageInfo {
  id: Stage;
  label: string;
  reward: number;
}

export type AiResult = 'validating' | 'accepted' | 'rejected' | null;

export interface TxLogEntry {
  id: string;
  hash: string;
  label: string;
  reward: number;
  datetime: string;
}

export interface DragState {
  bottle: InventoryBottleData;
  x: number;
  y: number;
  offsetX: number;
  offsetY: number;
}
