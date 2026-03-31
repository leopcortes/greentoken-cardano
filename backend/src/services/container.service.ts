import * as containersDb from '../db/queries/containers'
import { validateUUID } from '../validate'

/**
 * Registra volume depositado em um container.
 * Se o volume atingir a capacidade, marca como 'full'.
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

  // Marca como 'full' se atingiu a capacidade
  if (newVolume >= container.capacity_liters && container.status === 'active') {
    await containersDb.updateStatus(id, 'full')
    return { ...updated, status: 'full' }
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
 * Lista containers cheios prontos para rota.
 */
export async function listFull() {
  return containersDb.findByStatus('full')
}
