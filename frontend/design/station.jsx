/* ============ The shared interactive Station — used by all 3 variations ============
 * Props: variant ('studio' | 'pipeline' | 'player'), soundsOn
 *
 * Renders a 3-column layout, drag-from-inventory → drop-on-bin → process flow.
 * Variant changes layout emphasis + theming, NOT logic.
 */

const { useState, useEffect, useRef, useCallback, useMemo } = React;
const { STAGES, fakeTxHash, truncMid, buildInventory, makeReplacementBottle } = window.gtHelpers;

/* ============ small leaf components ============ */

const PipelineNodes = ({ activeIdx = -1, completedSet, layout = 'horizontal', size = 'md' }) => {
  // layout: 'horizontal' (pipeline-first) or 'compact' (sidebar)
  const dim = size === 'sm' ? { node: 28, gap: 28, font: 9 } : { node: 36, gap: 36, font: 10 };
  return (
    <div style={{ display: 'flex', alignItems: 'center', gap: 0, justifyContent: 'space-between', width: '100%' }}>
      {STAGES.map((s, i) => {
        const done = completedSet.has(s.id);
        const active = i === activeIdx;
        return (
          <React.Fragment key={s.id}>
            <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', gap: 6, flex: '0 0 auto' }}>
              <div style={{
                width: dim.node, height: dim.node, borderRadius: 999,
                background: done ? 'var(--gt-600)' : active ? 'var(--gt-100)' : 'var(--bg-elev)',
                border: `1.5px solid ${done ? 'var(--gt-600)' : active ? 'var(--gt-500)' : 'var(--line)'}`,
                display: 'flex', alignItems: 'center', justifyContent: 'center',
                color: done ? 'white' : active ? 'var(--gt-700)' : 'var(--ink-4)',
                fontSize: dim.font + 1, fontWeight: 700,
                transition: 'all 350ms cubic-bezier(.4,1.4,.5,1)',
                animation: active ? 'gt-node-pop 500ms ease' : 'none',
                boxShadow: active ? '0 0 0 4px rgba(34,197,94,0.15)' : 'none',
                position: 'relative',
              }}>
                {done ? (
                  <svg width="14" height="14" viewBox="0 0 14 14"><path d="M3 7 L6 10 L11 4" stroke="white" strokeWidth="2" fill="none" strokeLinecap="round" strokeLinejoin="round"/></svg>
                ) : (
                  i + 1
                )}
                {active && <span className="gt-pulse" style={{ position: 'absolute', top: -2, right: -2 }}/>}
              </div>
              <div style={{ fontSize: dim.font, fontWeight: 600, color: done || active ? 'var(--ink)' : 'var(--ink-4)', textAlign: 'center', lineHeight: 1.2 }}>{s.label}</div>
              <div className="mono" style={{ fontSize: dim.font - 1, color: done ? 'var(--gt-700)' : 'var(--ink-4)', fontWeight: 600 }}>+{s.reward}</div>
            </div>
            {i < STAGES.length - 1 && (
              <div style={{
                flex: 1, height: 2, minWidth: 16,
                background: done ? 'var(--gt-500)' : 'var(--line)',
                position: 'relative', top: -16,
                transition: 'background 400ms ease',
              }}/>
            )}
          </React.Fragment>
        );
      })}
    </div>
  );
};

const ConfettiBurst = ({ count = 14, originX = 0, originY = 0, targetX = -300, targetY = -200, onDone }) => {
  // green token coins fly toward wallet
  useEffect(() => {
    const t = setTimeout(() => onDone?.(), 1200);
    return () => clearTimeout(t);
  }, []);
  const items = useMemo(() => Array.from({ length: count }, (_, i) => ({
    i,
    dx: targetX + (Math.random() * 60 - 30),
    dy: targetY + (Math.random() * 40 - 20),
    delay: Math.random() * 180,
    size: 14 + Math.random() * 8,
  })), []);
  return (
    <div style={{ position: 'absolute', left: originX, top: originY, pointerEvents: 'none', zIndex: 50 }}>
      {items.map(p => (
        <div key={p.i} style={{
          position: 'absolute',
          width: p.size, height: p.size,
          borderRadius: 999,
          background: 'radial-gradient(circle at 35% 30%, #86efac, #15803d 80%)',
          boxShadow: '0 0 12px rgba(34,197,94,0.6), inset 0 -2px 0 rgba(0,0,0,0.2)',
          animation: `gt-coin-fly 900ms cubic-bezier(.5,0,.6,1) ${p.delay}ms forwards`,
          ['--tx']: `${p.dx}px`,
          ['--ty']: `${p.dy}px`,
          display: 'flex', alignItems: 'center', justifyContent: 'center',
          color: 'white', fontSize: 9, fontWeight: 700,
        }}>₲</div>
      ))}
    </div>
  );
};

const TxToast = ({ tx }) => (
  <div className="gt-card" style={{
    padding: '10px 12px', display: 'flex', gap: 10, alignItems: 'flex-start',
    boxShadow: 'var(--sh-2)', borderColor: 'var(--gt-200)', background: 'var(--gt-50)',
    animation: 'gt-node-pop 400ms ease',
  }}>
    <div style={{ marginTop: 2, width: 8, height: 8, borderRadius: 999, background: 'var(--gt-600)' }}/>
    <div style={{ flex: 1, minWidth: 0 }}>
      <div style={{ fontSize: 11, fontWeight: 700, color: 'var(--gt-800)', display: 'flex', justifyContent: 'space-between', gap: 8 }}>
        <span>{tx.label}</span>
        <span className="mono" style={{ fontWeight: 600, fontSize: 10 }}>+{tx.reward} ₲</span>
      </div>
      <div className="mono" style={{ fontSize: 10, color: 'var(--ink-3)', marginTop: 2 }}>{truncMid(tx.hash, 10, 8)}</div>
    </div>
  </div>
);

/* ============ Inventory bottle (draggable) ============ */
const InventoryBottle = ({ b, onPickStart, onPickEnd, disabled }) => {
  const ref = useRef(null);

  const handleDown = (e) => {
    if (disabled) return;
    e.preventDefault();
    const rect = ref.current.getBoundingClientRect();
    const offsetX = (e.clientX || e.touches?.[0].clientX) - rect.left;
    const offsetY = (e.clientY || e.touches?.[0].clientY) - rect.top;
    onPickStart(b, { offsetX, offsetY, startX: rect.left, startY: rect.top });
  };

  return (
    <div
      ref={ref}
      onMouseDown={handleDown}
      onTouchStart={handleDown}
      className="gt-grab"
      style={{
        ['--rot']: b.rot,
        animation: 'gt-idle-float 4s ease-in-out infinite',
        animationDelay: `${(parseFloat(b.id.replace(/\D/g,'')) % 7) * 0.2}s`,
        opacity: disabled ? 0.25 : 1,
        filter: disabled ? 'grayscale(0.6)' : 'none',
        transition: 'opacity 300ms, filter 300ms',
        display: 'inline-flex',
        transform: `rotate(${b.rot})`,
        touchAction: 'none',
      }}
    >
      <Bottle size={b.size} tint={b.tint} kind={b.kind} invalid={b.invalid}/>
    </div>
  );
};

/* ============ Main Station ============ */

const Station = ({ variant = 'studio', soundsOn = false }) => {
  const [inventory, setInventory] = useState(() => buildInventory(7));
  const [dragging, setDragging] = useState(null);    // { bottle, x, y, offsetX, offsetY }
  const [dropArmed, setDropArmed] = useState(false); // hovering bin
  const [lidOpen, setLidOpen] = useState(false);
  const [scanning, setScanning] = useState(false);
  const [reject, setReject] = useState(false);
  const [crushed, setCrushed] = useState(0);
  const [activeStage, setActiveStage] = useState(-1);   // current animating stage idx
  const [completed, setCompleted] = useState(new Set()); // stages confirmed for the current bottle
  const [currentBottle, setCurrentBottle] = useState(null); // bottle being processed (for left card)
  const [aiResult, setAiResult] = useState(null);       // 'validating' | 'accepted' | 'rejected' | null
  const [tokens, setTokens] = useState(127);            // wallet balance
  const [bumpKey, setBumpKey] = useState(0);
  const [confettiBursts, setConfettiBursts] = useState([]);
  const [txLog, setTxLog] = useState(() => [
    { id: 'init-1', hash: fakeTxHash(), label: 'Triturada', reward: 20, time: '14:02' },
    { id: 'init-2', hash: fakeTxHash(), label: 'Inserida',  reward: 10, time: '13:58' },
  ]);
  const [bottlesProcessed, setBottlesProcessed] = useState(8);
  const [fillPct, setFillPct] = useState(36);

  const binRef = useRef(null);
  const walletRef = useRef(null);

  const playBeep = useCallback((freq = 600, dur = 80, vol = 0.04) => {
    if (!soundsOn) return;
    try {
      const ctx = playBeep._ctx || (playBeep._ctx = new (window.AudioContext || window.webkitAudioContext)());
      const osc = ctx.createOscillator(); const g = ctx.createGain();
      osc.type = 'sine'; osc.frequency.value = freq;
      g.gain.value = vol; osc.connect(g); g.connect(ctx.destination);
      osc.start(); osc.stop(ctx.currentTime + dur / 1000);
    } catch {}
  }, [soundsOn]);

  /* ========== drag handlers ========== */
  const onPickStart = (bottle, meta) => {
    document.body.classList.add('gt-dragging');
    setDragging({ bottle, x: meta.startX, y: meta.startY, offsetX: meta.offsetX, offsetY: meta.offsetY });
  };

  useEffect(() => {
    if (!dragging) return;
    const onMove = (e) => {
      const px = e.clientX ?? e.touches?.[0]?.clientX;
      const py = e.clientY ?? e.touches?.[0]?.clientY;
      if (px == null) return;
      setDragging(d => d ? { ...d, x: px - d.offsetX, y: py - d.offsetY } : d);
      // hit-test bin
      if (binRef.current) {
        const r = binRef.current.getBoundingClientRect();
        const inside = px > r.left && px < r.right && py > r.top && py < r.bottom + 30;
        setDropArmed(inside);
        setLidOpen(inside);
      }
    };
    const onUp = () => {
      document.body.classList.remove('gt-dragging');
      const wasArmed = dropArmedRef.current;
      const droppedBottle = dragging.bottle;
      setDragging(null);
      setDropArmed(false);
      if (wasArmed) {
        handleDrop(droppedBottle);
      } else {
        setLidOpen(false);
      }
    };
    window.addEventListener('mousemove', onMove);
    window.addEventListener('touchmove', onMove, { passive: false });
    window.addEventListener('mouseup', onUp);
    window.addEventListener('touchend', onUp);
    return () => {
      window.removeEventListener('mousemove', onMove);
      window.removeEventListener('touchmove', onMove);
      window.removeEventListener('mouseup', onUp);
      window.removeEventListener('touchend', onUp);
    };
  }, [dragging]);

  // mirror dropArmed in a ref so onUp sees the current value
  const dropArmedRef = useRef(false);
  useEffect(() => { dropArmedRef.current = dropArmed; }, [dropArmed]);

  /* ========== drop / process pipeline ========== */
  const handleDrop = (bottle) => {
    setCurrentBottle(bottle);
    setAiResult('validating');
    setScanning(true);
    playBeep(880, 60);

    setTimeout(() => {
      setScanning(false);
      if (bottle.invalid) {
        // rejected
        setAiResult('rejected');
        setReject(true);
        setLidOpen(false);
        playBeep(220, 200, 0.05);
        // replenish inventory after a beat (the rejected one was never removed; just replace order)
        setTimeout(() => {
          setReject(false);
          setInventory(inv => {
            // remove this bottle and add a fresh one at the end
            const next = inv.filter(x => x.id !== bottle.id);
            next.push(makeReplacementBottle());
            return next;
          });
          // clear "current bottle" view after a moment
          setTimeout(() => { setCurrentBottle(null); setAiResult(null); }, 1400);
        }, 700);
        return;
      }

      // accepted — close lid, run pipeline
      setAiResult('accepted');
      setLidOpen(false);
      setInventory(inv => {
        const next = inv.filter(x => x.id !== bottle.id);
        next.push(makeReplacementBottle());
        return next;
      });
      setBottlesProcessed(n => n + 1);
      setFillPct(p => Math.min(100, p + 6));
      runPipelineForBottle(bottle);
    }, 900);
  };

  const runPipelineForBottle = (bottle) => {
    setCompleted(new Set());
    setActiveStage(0);
    let i = 0;
    const stepDelay = 1100;

    const stepNext = () => {
      const stage = STAGES[i];
      if (!stage) {
        setActiveStage(-1);
        // clear current bottle view after final stage so card resets to "esperando"
        setTimeout(() => { setCurrentBottle(null); setAiResult(null); }, 800);
        return;
      }

      // mid-stage side effects
      if (stage.id === 'compacted') {
        setCrushed(c => c + 1);
        playBeep(420, 100, 0.05);
      }

      // confirm tx after a beat
      setTimeout(() => {
        setCompleted(prev => new Set([...prev, stage.id]));
        // mint coin → wallet
        spawnConfetti();
        setTokens(t => t + stage.reward);
        setBumpKey(k => k + 1);
        playBeep(660 + i * 80, 90, 0.04);
        // toast tx
        setTxLog(log => [{
          id: `tx-${Date.now()}-${i}`,
          hash: fakeTxHash(),
          label: stage.label,
          reward: stage.reward,
          time: new Date().toTimeString().slice(0,5),
        }, ...log].slice(0, 12));

        i++;
        if (i < STAGES.length) {
          setActiveStage(i);
          setTimeout(stepNext, stepDelay);
        } else {
          setTimeout(() => setActiveStage(-1), 600);
        }
      }, stepDelay - 250);
    };
    stepNext();
  };

  const spawnConfetti = () => {
    if (!binRef.current || !walletRef.current) return;
    const binR = binRef.current.getBoundingClientRect();
    const walR = walletRef.current.getBoundingClientRect();
    const ox = binR.left + binR.width / 2;
    const oy = binR.top + binR.height * 0.25;
    const tx = walR.left + walR.width / 2 - ox;
    const ty = walR.top + walR.height / 2 - oy;
    const id = 'c-' + Date.now();
    setConfettiBursts(b => [...b, { id, ox, oy, tx, ty }]);
    setTimeout(() => setConfettiBursts(b => b.filter(x => x.id !== id)), 1300);
  };

  const resetInventory = () => {
    setInventory(buildInventory(Math.floor(Math.random() * 99)));
  };

  /* ========== layout per variant ========== */
  return (
    <div style={{
      width: '100%', height: '100%',
      background: 'var(--bg)', color: 'var(--ink)',
      padding: 24, display: 'flex', flexDirection: 'column', gap: 16,
      position: 'relative', overflow: 'hidden',
    }} className={variant === 'player' ? 'gt-dark' : ''}>

      {/* ---- top bar ---- */}
      <TopBar variant={variant} bottlesProcessed={bottlesProcessed} resetInventory={resetInventory} />

      {/* ---- pipeline-first variation puts the pipeline above ---- */}
      {variant === 'pipeline' && (
        <div className="gt-card" style={{ padding: '18px 22px' }}>
          <div className="gt-eyebrow" style={{ marginBottom: 12 }}>Pipeline on-chain · Cardano testnet</div>
          <PipelineNodes activeIdx={activeStage} completedSet={completed} size="md"/>
        </div>
      )}

      {/* ---- main 3-column layout ---- */}
      <div style={{
        flex: 1, display: 'grid',
        gridTemplateColumns: variant === 'pipeline' ? '320px 1fr 360px' : '340px 1fr 360px',
        gap: 16, minHeight: 0,
      }}>
        {/* LEFT: bottle details + wallet */}
        <div style={{ display: 'flex', flexDirection: 'column', gap: 14, minHeight: 0 }}>
          <BottleDetailCard variant={variant} currentBottle={currentBottle} aiResult={aiResult} activeStage={activeStage} completed={completed}/>
          <WalletCard variant={variant} tokens={tokens} bumpKey={bumpKey} txLog={txLog} walletRef={walletRef}/>
        </div>

        {/* CENTER: bin + drop zone */}
        <div style={{
          position: 'relative', display: 'flex', flexDirection: 'column',
          alignItems: 'center', justifyContent: 'center',
          background: variant === 'player'
            ? 'radial-gradient(ellipse at 50% 60%, rgba(34,197,94,0.10), transparent 60%)'
            : 'radial-gradient(ellipse at 50% 70%, rgba(22,163,74,0.06), transparent 60%)',
          borderRadius: 'var(--r-xl)',
          border: '1px dashed var(--line)',
          padding: 24,
        }}>
          {variant !== 'pipeline' && (
            <div style={{ position: 'absolute', top: 18, left: 22, right: 22, display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
              <span className="gt-eyebrow">Container · Pão de Açúcar 712</span>
              <span className="gt-chip gt-chip--green"><span className="gt-pulse"/>Online</span>
            </div>
          )}
          {variant === 'pipeline' && (
            <div style={{ position: 'absolute', top: 18, left: 22, right: 22, display: 'flex', justifyContent: 'center' }}>
              <span className="gt-chip gt-chip--ghost">Arraste uma garrafa para o container</span>
            </div>
          )}

          <div ref={binRef} style={{
            position: 'relative', width: 320, height: 380, marginTop: 30,
            transition: 'transform 300ms ease',
            transform: dropArmed ? 'scale(1.04)' : 'scale(1)',
            animation: dropArmed ? 'gt-drop-glow 1.2s ease-in-out infinite' : 'none',
            borderRadius: 16,
          }}>
            <Container open={lidOpen} scanning={scanning} reject={reject} fillPct={fillPct} crushedCount={crushed} variant={variant}/>
            {/* squashing bottle ghost when an item is being processed */}
            {scanning && <ScanBadge/>}
          </div>

          {/* hint */}
          <div style={{ marginTop: 18, fontSize: 12, color: 'var(--ink-3)', textAlign: 'center', maxWidth: 320 }}>
            {activeStage >= 0
              ? <span><strong style={{ color: 'var(--gt-700)' }}>Processando…</strong> {STAGES[activeStage]?.label}</span>
              : 'Toque ou arraste uma garrafa do inventário para iniciar.'}
          </div>

          {/* enlarged volume bar below bin (studio + player) */}
          {variant !== 'pipeline' && (
            <div style={{ marginTop: 22, width: '100%', maxWidth: 460 }}>
              <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'baseline', marginBottom: 8 }}>
                <span className="gt-eyebrow">Volume do container</span>
                <span className="mono" style={{ fontSize: 12, fontWeight: 700, color: fillPct >= 90 ? 'var(--warn)' : 'var(--ink-2)' }}>{fillPct}%</span>
              </div>
              <div style={{
                position: 'relative', height: 14, borderRadius: 999,
                background: 'var(--bg-elev)', border: '1px solid var(--line)',
                overflow: 'hidden',
              }}>
                <div style={{
                  position: 'absolute', inset: 0,
                  width: `${fillPct}%`,
                  background: fillPct >= 90
                    ? 'linear-gradient(90deg, var(--gt-500), #f59e0b)'
                    : 'linear-gradient(90deg, var(--gt-400), var(--gt-600))',
                  borderRadius: 999,
                  transition: 'width 600ms cubic-bezier(.4,1.2,.6,1)',
                  boxShadow: 'inset 0 1px 0 rgba(255,255,255,0.35)',
                }}/>
                {/* 90% threshold marker */}
                <div style={{
                  position: 'absolute', top: -2, bottom: -2, left: '90%',
                  width: 1, background: 'var(--ink-3)', opacity: 0.4,
                }}/>
              </div>
              <div style={{ display: 'flex', justifyContent: 'space-between', marginTop: 6, fontSize: 10, color: 'var(--ink-4)' }}>
                <span>0%</span>
                <span>90% — pronto para coleta</span>
                <span>100%</span>
              </div>
            </div>
          )}
        </div>

        {/* RIGHT: inventory */}
        <Inventory inventory={inventory} onPickStart={onPickStart} disabled={!!dragging || activeStage >= 0} variant={variant}/>
      </div>

      {/* ---- floating dragged bottle ---- */}
      {dragging && (
        <div style={{
          position: 'fixed', left: dragging.x, top: dragging.y,
          pointerEvents: 'none', zIndex: 100,
          filter: 'drop-shadow(0 18px 24px rgba(14,20,16,0.25))',
          transform: dropArmed ? 'scale(1.15)' : 'scale(1.05)',
          transition: 'transform 180ms ease',
        }}>
          <Bottle size={dragging.bottle.size} tint={dragging.bottle.tint} kind={dragging.bottle.kind} invalid={dragging.bottle.invalid}/>
        </div>
      )}

      {/* ---- token confetti bursts ---- */}
      {confettiBursts.map(c => (
        <ConfettiBurst key={c.id} originX={c.ox} originY={c.oy} targetX={c.tx} targetY={c.ty} onDone={() => {}}/>
      ))}
    </div>
  );
};

const ScanBadge = () => (
  <div style={{
    position: 'absolute', top: 70, left: '50%', transform: 'translateX(-50%)',
    background: 'rgba(20,83,45,0.92)', color: '#bbf7d0',
    padding: '4px 10px', borderRadius: 6, fontSize: 10, fontWeight: 600,
    letterSpacing: '0.08em', textTransform: 'uppercase',
    fontFamily: 'var(--font-mono)',
    border: '1px solid rgba(187,247,208,0.3)',
  }}>
    AI · Identificando…
  </div>
);

/* ============ Sub-cards ============ */

const BottleDetailCard = ({ variant, currentBottle, aiResult, activeStage, completed }) => {
  // 3 status rows the user cares about at the container moment
  // status: 'pending' | 'active' | 'done' | 'failed'
  let aiStatus = 'pending';
  if (aiResult === 'validating') aiStatus = 'active';
  else if (aiResult === 'accepted') aiStatus = 'done';
  else if (aiResult === 'rejected') aiStatus = 'failed';

  const insertedDone = completed.has('inserted');
  const insertedActive = activeStage === 0;
  const insertedStatus = aiResult === 'rejected' ? 'pending'
    : insertedDone ? 'done' : insertedActive ? 'active' : 'pending';

  const compactedDone = completed.has('compacted');
  const compactedActive = activeStage === 1;
  const compactedStatus = aiResult === 'rejected' ? 'pending'
    : compactedDone ? 'done' : compactedActive ? 'active' : 'pending';

  // bottle metadata
  const sizeLabel = currentBottle ? (currentBottle.size === 'S' ? '300 ml' : currentBottle.size === 'L' ? '2 L' : '600 ml') : '—';
  const kindLabel = currentBottle ? (currentBottle.invalid === 'can' ? 'Alumínio' : currentBottle.invalid === 'glass' ? 'Vidro' : currentBottle.kind) : '—';
  const tintLabel = currentBottle ? ({clear:'Transparente', green:'Verde', blue:'Azul', amber:'Âmbar'}[currentBottle.tint] || '—') : '—';
  const accepted = currentBottle && !currentBottle.invalid;

  return (
    <div className="gt-card" style={{ padding: 18 }}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'baseline', marginBottom: 14 }}>
        <div className="gt-eyebrow">Garrafa atual</div>
        {currentBottle && (
          <span className="mono" style={{ fontSize: 10, color: 'var(--ink-4)' }}>#{currentBottle.id.slice(-6)}</span>
        )}
      </div>

      {/* preview + meta */}
      <div style={{
        display: 'flex', gap: 14, alignItems: 'center',
        padding: 14, borderRadius: 12,
        background: currentBottle
          ? (accepted ? 'var(--gt-50)' : 'var(--err-soft)')
          : 'var(--bg)',
        border: `1px solid ${currentBottle ? (accepted ? 'var(--gt-200)' : '#fecaca') : 'var(--line)'}`,
        transition: 'background 300ms, border-color 300ms',
        minHeight: 110,
      }}>
        <div style={{ flex: '0 0 auto', display: 'flex', alignItems: 'center', justifyContent: 'center', width: 70, height: 90 }}>
          {currentBottle ? (
            <Bottle size={currentBottle.size} tint={currentBottle.tint} kind={currentBottle.kind} invalid={currentBottle.invalid}/>
          ) : (
            <div style={{
              width: 36, height: 70, borderRadius: 8,
              border: '1.5px dashed var(--line)',
              display: 'flex', alignItems: 'center', justifyContent: 'center',
              color: 'var(--ink-4)', fontSize: 18,
            }}>?</div>
          )}
        </div>
        <div style={{ flex: 1, minWidth: 0 }}>
          {currentBottle ? (
            <>
              <div style={{ fontSize: 16, fontWeight: 700, color: 'var(--ink)', letterSpacing: '-0.01em' }}>
                {kindLabel} · {sizeLabel}
              </div>
              <div style={{ fontSize: 11, color: 'var(--ink-3)', marginTop: 4, lineHeight: 1.5 }}>
                Cor: {tintLabel}<br/>
                Material: {accepted ? 'Reciclável (PET/HDPE)' : currentBottle.invalid === 'can' ? 'Alumínio (não aceito)' : 'Vidro (não aceito)'}
              </div>
            </>
          ) : (
            <>
              <div style={{ fontSize: 14, fontWeight: 600, color: 'var(--ink-2)' }}>Aguardando inserção</div>
              <div style={{ fontSize: 11, color: 'var(--ink-3)', marginTop: 4, lineHeight: 1.5 }}>
                Arraste uma garrafa do inventário para o container ao lado.
              </div>
            </>
          )}
        </div>
      </div>

      {/* status rows */}
      <div style={{ marginTop: 14, display: 'flex', flexDirection: 'column', gap: 4 }}>
        <StatusRow
          label="Validada pela IA"
          status={aiStatus}
          activeText="Identificando…"
          doneText="Aceita"
          failedText={`Rejeitada · não é PET`}
        />
        <StatusRow
          label="Inserida no container"
          status={insertedStatus}
          activeText="Confirmando on-chain…"
          doneText={`+10 ₲ creditados`}
        />
        <StatusRow
          label="Compactada"
          status={compactedStatus}
          activeText="Aguardando ciclo…"
          doneText={`+3 ₲ creditados`}
        />
      </div>

      <div style={{ marginTop: 12, paddingTop: 12, borderTop: '1px solid var(--line)', fontSize: 10, color: 'var(--ink-4)', lineHeight: 1.5 }}>
        Próximas etapas (coleta, estação, trituração) acontecem fora do container e renderão até <span style={{ color: 'var(--gt-700)', fontWeight: 700 }}>+37 ₲</span> a mais.
      </div>
    </div>
  );
};

const StatusRow = ({ label, status, activeText, doneText, failedText }) => {
  const styles = {
    pending: { bg: 'transparent', icon: 'var(--bg-elev)', iconBd: 'var(--line)', iconCol: 'var(--ink-4)', txt: 'var(--ink-4)', sub: '' },
    active:  { bg: 'var(--gt-50)', icon: 'white',         iconBd: 'var(--gt-500)', iconCol: 'var(--gt-700)', txt: 'var(--ink)',  sub: activeText },
    done:    { bg: 'transparent',  icon: 'var(--gt-600)', iconBd: 'var(--gt-600)', iconCol: 'white',          txt: 'var(--ink)',  sub: doneText },
    failed:  { bg: 'var(--err-soft)', icon: 'var(--err)', iconBd: 'var(--err)',  iconCol: 'white',          txt: 'var(--ink)',  sub: failedText },
  };
  const s = styles[status];

  return (
    <div style={{
      display: 'flex', alignItems: 'center', gap: 12,
      padding: '8px 10px', borderRadius: 8,
      background: s.bg,
      transition: 'background 240ms',
    }}>
      <div style={{
        width: 22, height: 22, borderRadius: 999,
        background: s.icon, border: `1.5px solid ${s.iconBd}`,
        display: 'flex', alignItems: 'center', justifyContent: 'center',
        flex: '0 0 auto', position: 'relative',
      }}>
        {status === 'done' && (
          <svg width="11" height="11" viewBox="0 0 14 14"><path d="M3 7 L6 10 L11 4" stroke={s.iconCol} strokeWidth="2.2" fill="none" strokeLinecap="round" strokeLinejoin="round"/></svg>
        )}
        {status === 'failed' && (
          <svg width="10" height="10" viewBox="0 0 14 14"><path d="M3 3 L11 11 M11 3 L3 11" stroke={s.iconCol} strokeWidth="2.2" strokeLinecap="round"/></svg>
        )}
        {status === 'active' && <span className="gt-pulse" style={{ position: 'absolute', inset: 'auto' }}/>}
      </div>
      <div style={{ flex: 1, minWidth: 0 }}>
        <div style={{ fontSize: 12, fontWeight: 600, color: s.txt, lineHeight: 1.3 }}>{label}</div>
        {s.sub && (
          <div style={{
            fontSize: 10, marginTop: 2,
            color: status === 'failed' ? 'var(--err)' : status === 'done' ? 'var(--gt-700)' : 'var(--ink-3)',
            fontWeight: status === 'done' ? 600 : 400,
          }}>{s.sub}</div>
        )}
      </div>
    </div>
  );
};

const TopBar = ({ variant, bottlesProcessed, resetInventory }) => (
  <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between', gap: 16 }}>
    <div style={{ display: 'flex', alignItems: 'center', gap: 12 }}>
      <div style={{
        width: 36, height: 36, borderRadius: 10,
        background: 'var(--gt-600)', color: 'white',
        display: 'flex', alignItems: 'center', justifyContent: 'center',
        fontWeight: 800, fontSize: 16, letterSpacing: '-0.04em',
        boxShadow: '0 4px 12px rgba(22,163,74,0.4)',
      }}>₲</div>
      <div>
        <div style={{ fontSize: 14, fontWeight: 700, letterSpacing: '-0.01em' }}>Greentoken Station</div>
        <div style={{ fontSize: 11, color: 'var(--ink-3)' }}>
          {variant === 'studio' && 'Estúdio · interação calma'}
          {variant === 'pipeline' && 'Pipeline-first · ênfase blockchain'}
          {variant === 'player' && 'Player mode · noturno'}
        </div>
      </div>
    </div>
    <div style={{ display: 'flex', gap: 10, alignItems: 'center' }}>
      <span className="gt-chip gt-chip--ghost"><span className="mono">{bottlesProcessed}</span> hoje</span>
      <span className="gt-chip gt-chip--cdn">Cardano · preprod</span>
      <a
        href="http://localhost:5173"
        target="_blank"
        rel="noopener"
        style={{
          display: 'inline-flex', alignItems: 'center', gap: 6,
          padding: '6px 12px', borderRadius: 8,
          background: 'var(--ink)', color: 'var(--bg-card)',
          fontSize: 11, fontWeight: 600, letterSpacing: '0.01em',
          textDecoration: 'none', cursor: 'pointer',
          border: '1px solid var(--ink)',
          transition: 'opacity 200ms',
        }}
        onMouseEnter={(e) => e.currentTarget.style.opacity = '0.85'}
        onMouseLeave={(e) => e.currentTarget.style.opacity = '1'}
      >
        Dashboard
        <svg width="11" height="11" viewBox="0 0 12 12" fill="none">
          <path d="M3 3 L9 3 L9 9 M9 3 L3 9" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round"/>
        </svg>
      </a>
    </div>
  </div>
);

const WalletCard = ({ variant, tokens, bumpKey, txLog, walletRef }) => {
  const tier = tokens >= 500 ? { name: 'Ouro', color: '#a16207', soft: '#fef3c7' } :
               tokens >= 200 ? { name: 'Prata', color: '#475569', soft: '#e2e8f0' } :
                               { name: 'Bronze', color: '#9a3412', soft: '#fed7aa' };
  return (
    <div ref={walletRef} className="gt-card" style={{ padding: 18, flex: 1, minHeight: 0, display: 'flex', flexDirection: 'column' }}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start', marginBottom: 14 }}>
        <div>
          <div className="gt-eyebrow">Carteira Cardano</div>
          <div className="mono" style={{ fontSize: 10, color: 'var(--ink-4)', marginTop: 4, display: 'flex', alignItems: 'center', gap: 4 }}>
            addr1q9x…m4tu
            <button style={{ background: 'transparent', border: 'none', cursor: 'pointer', color: 'var(--ink-3)', padding: 0, fontSize: 10 }}>⧉</button>
          </div>
        </div>
        <div style={{
          display: 'flex', alignItems: 'center', gap: 6,
          padding: '4px 8px', borderRadius: 999,
          background: tier.soft, border: `1px solid ${tier.color}33`,
          fontSize: 10, fontWeight: 700, color: tier.color, letterSpacing: '0.04em',
        }}>
          ◆ {tier.name}
        </div>
      </div>

      <div style={{ display: 'flex', alignItems: 'baseline', gap: 8, marginBottom: 4 }}>
        <span key={bumpKey} className="gt-bump" style={{
          fontSize: 44, fontWeight: 800, letterSpacing: '-0.03em', color: 'var(--ink)',
          fontFamily: 'var(--font-mono)', display: 'inline-block',
        }}>{tokens}</span>
        <span style={{ fontSize: 16, fontWeight: 700, color: 'var(--gt-700)' }}>₲</span>
      </div>
      <div style={{ fontSize: 11, color: 'var(--ink-3)', marginBottom: 14 }}>
        ≈ R$ {(tokens * 0.05).toFixed(2)} em vouchers
      </div>

      <div style={{ display: 'flex', gap: 8, marginBottom: 14 }}>
        <button style={{
          flex: 1, padding: '8px 10px', borderRadius: 8,
          background: 'var(--gt-600)', color: 'white', border: 'none',
          fontSize: 12, fontWeight: 600, cursor: 'pointer',
          boxShadow: '0 2px 6px rgba(22,163,74,0.3)',
        }}>Resgatar</button>
        <button style={{
          flex: 1, padding: '8px 10px', borderRadius: 8,
          background: 'transparent', color: 'var(--ink-2)', border: '1px solid var(--line)',
          fontSize: 12, fontWeight: 600, cursor: 'pointer',
        }}>Histórico</button>
      </div>

      <div style={{ borderTop: '1px solid var(--line)', paddingTop: 12, flex: 1, minHeight: 0, display: 'flex', flexDirection: 'column' }}>
        <div className="gt-eyebrow" style={{ marginBottom: 8 }}>Últimas recompensas</div>
        <div style={{ display: 'flex', flexDirection: 'column', gap: 6, overflow: 'auto' }} className="gt-no-scrollbar">
          {txLog.slice(0, 6).map(tx => (
            <div key={tx.id} style={{
              display: 'flex', justifyContent: 'space-between', alignItems: 'center', gap: 8,
              padding: '6px 0', fontSize: 11,
            }}>
              <div style={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
                <span style={{ fontWeight: 600, color: 'var(--ink-2)' }}>{tx.label}</span>
                <span className="mono" style={{ color: 'var(--ink-4)', fontSize: 9 }}>{truncMid(tx.hash, 8, 6)}</span>
              </div>
              <div className="mono" style={{ fontWeight: 700, color: 'var(--gt-700)', fontSize: 12 }}>+{tx.reward}</div>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
};

const Inventory = ({ inventory, onPickStart, disabled, variant }) => (
  <div className="gt-card" style={{ padding: 18, display: 'flex', flexDirection: 'column', minHeight: 0 }}>
    <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'baseline', marginBottom: 6 }}>
      <div className="gt-eyebrow">Inventário</div>
      <span className="gt-chip gt-chip--ghost">{inventory.length} itens</span>
    </div>
    <div style={{ fontSize: 11, color: 'var(--ink-3)', marginBottom: 14 }}>
      Arraste uma garrafa para o container. Itens não-PET são rejeitados pela IA.
    </div>
    <div style={{
      flex: 1, minHeight: 0,
      display: 'grid',
      gridTemplateColumns: 'repeat(4, 1fr)',
      gridAutoRows: 'minmax(96px, auto)',
      gap: 6,
      alignContent: 'start',
      overflow: 'auto',
      padding: '10px 6px',
      background: variant === 'player' ? 'rgba(255,255,255,0.02)' : 'var(--bg)',
      borderRadius: 12,
      border: '1px dashed var(--line)',
    }} className="gt-no-scrollbar">
      {inventory.map(b => (
        <div key={b.id} style={{ display: 'flex', justifyContent: 'center', alignItems: 'flex-end', minHeight: 96 }}>
          <InventoryBottle b={b} onPickStart={onPickStart} disabled={disabled}/>
        </div>
      ))}
    </div>
  </div>
);

window.Station = Station;
