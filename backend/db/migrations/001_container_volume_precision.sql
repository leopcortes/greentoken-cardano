-- Aumenta a precisão do volume armazenado do container (NUMERIC(10,2) -> NUMERIC(12,5)).
-- Garrafas de volumes variados (300ml, 600ml, 2L) combinadas com fator de compactação
-- aleatório geram deltas que ultrapassam 2 casas decimais - armazenamos com mais
-- precisão e arredondamos apenas na exibição.
ALTER TABLE containers
  ALTER COLUMN current_volume_liters TYPE NUMERIC(12,5);
