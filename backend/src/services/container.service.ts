import * as containersDb from '../db/queries/containers'
import { validateUUID } from '../validate'

/**
 * Registra volume depositado em um container.
 * Se atingir 90% da capacidade, marca como 'ready_for_collection' (pronto para coleta).
 */
export async function addVolume(containerId: string, liters: number) {
  const id = validateUUID(containerId)

  const container = await containersDb.findById(id)
  if (!container) throw new Error(`Container nao encontrado: ${id}`)
  if (container.status === 'maintenance') throw new Error('Container em manutencao')

  const newVolume = Math.min(
    container.current_volume_liters + liters,
    container.capacity_liters,
  )

  const updated = await containersDb.updateVolume(id, newVolume)

  const fillPercent = (newVolume / container.capacity_liters) * 100
  if (fillPercent >= 90 && container.status === 'active') {
    await containersDb.updateStatus(id, 'ready_for_collection')
    return { ...updated, status: 'ready_for_collection' }
  }

  return updated
}

/**
 * Reseta o container apos coleta (caminhao esvaziou).
 */
export async function markCollected(containerId: string) {
  const id = validateUUID(containerId)
  await containersDb.updateVolume(id, 0)
  await containersDb.updateStatus(id, 'active')
}

/**
 * Lista containers prontos para rota (90%+ cheios).
 */
export async function listReadyForCollection() {
  return containersDb.findByStatus('ready_for_collection')
}
