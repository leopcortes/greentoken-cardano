# Greentoken Station — Protótipo de design

Protótipo interativo da tela frontal do container Greentoken (versão "Estúdio").

## Como abrir localmente

Abra `Greentoken Station.html` em um navegador. Tudo é estático (HTML + CSS + JSX via Babel-in-browser). Sem build, sem npm install.

> Em produção este protótipo deve ser portado para React/TypeScript dentro do app Vite existente em `frontend/`.

## Arquivos

| Arquivo | Função |
|---|---|
| `Greentoken Station.html` | Entry point — carrega React + Babel + os módulos JSX |
| `styles.css` | Design tokens (cores, tipografia, animações, sombras) |
| `shared.jsx` | Componente `Bottle` (SVG da garrafa PET com variantes), inventário fixo, helpers (STAGES, fakeTxHash, truncMid) |
| `container.jsx` | Componente `Container` — SVG da lixeira com tampa animada, scan da IA, compactação interna |
| `station.jsx` | Componente principal `Station` — layout 3 colunas, drag-drop, pipeline de estágios, wallet, confete |
| `tweaks-panel.jsx` | Painel flutuante de tweaks (liga/desliga sons) — pode ser ignorado em produção |

## Notas de comportamento

**Fluxo do drop:**
1. Usuário arrasta uma garrafa do inventário (direita) para o container (centro)
2. Tampa do container abre no hover
3. Drop → IA scaneia (linha varrendo, ~900ms) → aceita ou rejeita
4. Se aceita: lixeira fecha, garrafa some do inventário (reposta automaticamente), pipeline avança
5. Pipeline: `inserted → compacted → collected → atstation → shredded` (1.1s entre estágios)
6. A cada estágio confirmado: confete de tokens voa do container até a wallet, contador anima count-up
7. Itens não-PET (lata, vidro) → rejeitados com shake vermelho

**Inventário:** 20 itens fixos. Quando uma garrafa é arrastada (aceita ou rejeitada), uma nova é adicionada ao final.

**Card "Garrafa atual" (esquerda-cima):** mostra apenas as 3 etapas relevantes na frente do container (Validada IA, Inserida, Compactada). As outras 2 etapas (coleta, estação, trituração) acontecem fora e são apenas mencionadas no rodapé.

**Wallet (esquerda-baixo):** saldo Greentoken, endereço Cardano truncado com botão copiar, tier (Bronze/Prata/Ouro) baseado no saldo, lista das últimas tx hashes.

**Volume bar:** barra abaixo da lixeira mostra preenchimento, com marker em 90% (limite para coleta).

**Botão "Dashboard"** (topo direita): abre `http://localhost:5173` em nova aba — endereço do dashboard CRUD existente em `frontend/`. Ajuste a URL se necessário.

## Design tokens (de `styles.css`)

- **Verde Greentoken** — `--gt-600: #16a34a` (primário, mesmo do dashboard)
- **Cardano blue** — `--cdn: #0033AD` (apenas em badges on-chain)
- **Off-white quente** — `--bg: #f7f6f1`
- **Tipografia** — Geist (UI) + Geist Mono (tx hashes, valores numéricos)

## Sugestão para implementar como página React/TS

1. Crie a rota em `frontend/src/pages/StationPage.tsx`
2. Porte `Container` e `Bottle` SVGs como componentes `.tsx` (puro markup — fácil)
3. Porte a lógica de drag/drop e pipeline de `station.jsx` para hooks (`useStation`, `useDrag`)
4. Substitua o `fakeTxHash()` por chamadas reais à API:
   - Drop válido → `POST /bottles` (já existe em `services/api.ts`)
   - Pipeline → polling de `GET /bottles/:id` para detectar transições de estágio confirmadas
   - Wallet → `GET /users/:id/rewards`
5. Use os tokens de `styles.css` ou converta para variáveis CSS no `index.css` do app
