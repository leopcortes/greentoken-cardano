-- Remove rewards duplicadas mantendo apenas a mais antiga por (bottle_id, stage).
-- Causa: a tabela não tinha constraint UNIQUE, permitindo que o worker criasse
-- múltiplos registros caso shredStation/processTransaction fosse invocado mais
-- de uma vez para o mesmo estágio da mesma garrafa.
DELETE FROM rewards
WHERE id NOT IN (
  SELECT DISTINCT ON (bottle_id, stage) id
  FROM rewards
  ORDER BY bottle_id, stage, sent_at ASC
);

-- Impede duplicatas futuras a nível de banco de dados.
ALTER TABLE rewards
  ADD CONSTRAINT rewards_bottle_stage_unique UNIQUE (bottle_id, stage);
