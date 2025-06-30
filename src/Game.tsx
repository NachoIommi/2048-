import { useEffect, useState } from 'react';
import PengineClient, { PrologTerm } from './PengineClient';
import Board from './Board';
import Block from './Block';
import { delay } from './util';

export type Grid = (number | "-")[];

interface EffectTerm extends PrologTerm {
  functor: "effect";
  args: [Grid, EffectInfoTerm[]];
}

type EffectInfoTerm = NewBlockTerm | ScoreTerm | SideBlockTerm | PrologTerm;

interface SideBlockTerm extends PrologTerm {
  functor: "sideBlock";
  args: [number, number];
}

interface NewBlockTerm extends PrologTerm {
  functor: "newBlock";
  args: [number];
}

interface ScoreTerm extends PrologTerm {
  functor: "score";
  args: [number];
}

interface Notification {
  id: number;
  msg: string;
  type: 'central' | 'combo' | 'good' | 'excellent' | 'eliminated'; // Añadimos el tipo para facilitar el filtrado
}

function Game() {
  const [pengine, setPengine] = useState<any>(null);
  const [grid, setGrid] = useState<Grid | null>(null);
  const [numOfColumns, setNumOfColumns] = useState<number | null>(null);
  const [score, setScore] = useState<number>(0);
  const [shootBlock, setShootBlock] = useState<number | null>(null);
  const [nextBlock, setNextBlock] = useState<number | null>(null);
  const [waiting, setWaiting] = useState<boolean>(false);
  const [fusionGroup, setFusionGroup] = useState<number[]>([]);
  const [animateNextBlock, setAnimateNextBlock] = useState(false);
  const [isNextBlockRevealed, setIsNextBlockRevealed] = useState(false);
  const [revealProgress, setRevealProgress] = useState(0);
  const [highestBlockReached, setHighestBlockReached] = useState<number>(0);
  const [maxShooteable, setMaxShooteable] = useState<number>(0);
  const [forbiddenBlocks, setForbiddenBlocks] = useState<number[]>([]);
  const [notifications, setNotifications] = useState<Notification[]>([]);
  const [notificationCounter, setNotificationCounter] = useState(0);
  const [gameOver, setGameOver] = useState(false);
  

  // Modificación en pushNotification para incluir el tipo 'eliminated'
  function pushNotification(msg: string, customType?: Notification['type']) {
    setNotificationCounter(id => {
      const newId = id + 1;
      let type: Notification['type'] = 'central'; // Por defecto es central

      if (msg.includes('Combo')) {
        type = 'combo';
      } else if (msg.includes('Good!')) {
        type = 'good';
      } else if (msg.includes('Excellent!')) {
        type = 'excellent';
      } else if (msg.includes('Eliminado')) { 
        type = 'eliminated';
      }

      setNotifications(prev => [...prev, { id: newId, msg, type }]);
      setTimeout(() => {
        setNotifications(prev => prev.filter(n => n.id !== newId));
      }, 5000); // 5 segundos para que coincida con la animación
      return newId;
    });
  }

  useEffect(() => { connectToPenginesServer(); }, []);
  useEffect(() => { if (pengine) initGame(); }, [pengine]);

  async function connectToPenginesServer() {
    setPengine(await PengineClient.create());
  }

  async function initGame() {
    const res = await pengine!.query('init(Grid, NumOfColumns), randomBlock(Grid, Block1), randomBlock(Grid,Block2), forbidden_blocks_accumulated(Forbidden)');
    setGrid(res['Grid']);
    setShootBlock(res['Block1']);
    setNextBlock(res['Block2']);
    setNumOfColumns(res['NumOfColumns']);
    setHighestBlockReached(0);
    setMaxShooteable(0);
    setNotifications([]); // Limpiar notificaciones al iniciar
    setNotificationCounter(0); // Reiniciar contador
    setGameOver(false);
  }

  async function handleLaneClick(lane: number) {
    if (waiting || nextBlock === null) return;

    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const queryS = `shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects, ForbiddenBlocksOut), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block)`;
    setWaiting(true);

    const response = await pengine.query(queryS);
    if (response) {
      animateEffect(response['Effects']);
      setAnimateNextBlock(true);
      await delay(300);
      setShootBlock(nextBlock);
      setNextBlock(response['Block']);
      setAnimateNextBlock(false);

      // 1. Obtener los bloques prohibidos ANTES de este turno (si tienes un estado que los guarda)
        // Para simplificar, vamos a comparar con el estado actual `forbiddenBlocks`
        const oldForbiddenBlocks = forbiddenBlocks;
        const newForbiddenBlocks = response['ForbiddenBlocksOut'];

        // 2. Actualizar el estado con los nuevos bloques prohibidos
        setForbiddenBlocks(newForbiddenBlocks);

        // 3. Detectar qué bloques son nuevos en la lista de prohibidos
        const newlyForbidden = newForbiddenBlocks.filter(
            (block: number) => !oldForbiddenBlocks.includes(block)
        );

        // 4. Disparar notificaciones por cada bloque recién prohibido
        newlyForbidden.forEach((block: number) => {
            pushNotification(`🚫 Bloque Eliminado: ${block}!`, 'eliminated'); // Usa tu tipo 'eliminated'
        });

    } else {
      setWaiting(false);
    }
  }

  async function animateEffect(effects: EffectTerm[]) {
    let prevGrid = grid;

    for (const effect of effects) {
      const [effectGrid, effectInfo] = effect.args;

      if (!effectGrid.includes('-')) {
        setGrid(effectGrid);
        setGameOver(true);
        return;
      }

      setGrid(effectGrid);

      // Detectar fusión
      let fusionIdx: number | null = null;
      if (prevGrid) {
        for (let j = 0; j < effectGrid.length; j++) {
          const prev = prevGrid[j], curr = effectGrid[j];
          if (prev !== '-' && curr !== '-' && Number(curr) > Number(prev)) {
            fusionIdx = j;
            break;
          }
        }
      }

      if (fusionIdx !== null && prevGrid && numOfColumns) {
        const targetVal = prevGrid[fusionIdx];
        const queue = [fusionIdx];
        const visited = new Set<number>([fusionIdx]);
        const cluster: number[] = [];

        while (queue.length) {
          const idx = queue.shift()!;
          cluster.push(idx);
          const neighbors = [
            idx - numOfColumns, idx + numOfColumns,
            (idx % numOfColumns !== 0) ? idx - 1 : -1,
            ((idx + 1) % numOfColumns !== 0) ? idx + 1 : -1
          ];
          for (const n of neighbors) {
            if (n >= 0 && n < prevGrid.length && !visited.has(n) && prevGrid[n] === targetVal) {
              visited.add(n);
              queue.push(n);
            }
          }
        }

        setFusionGroup(cluster);
        await delay(500);
        setFusionGroup([]);

        // Ahora pushNotification usará el nuevo tipo
        if (cluster.length > 2) {
          pushNotification(`🔥 Combo x ${cluster.length}`);
          if (cluster.length === 3) pushNotification('👍 Good!');
          else if (cluster.length === 4) pushNotification('✨ Excellent!');
        }
      }

      effectInfo.forEach(({ functor, args }) => {
        const val = args[0];

        if (functor === 'newBlock') {
          setScore(s => s + val); // Añadir al score por newBlock
          if (val > highestBlockReached) {
            setHighestBlockReached(val);
            pushNotification(`🎉 Nuevo Bloque Maximo Alcanzado: ${val}`);
          }
        }

        if (functor === 'score') {
          setScore(s => s + val); // Añadir al score por score (puntos ganados)
        }
      });

      prevGrid = effectGrid;
      await delay(300);
    }

    // Consultar nuevo maxShooteable
    const finalGridS = JSON.stringify(prevGrid).replace(/"/g, '');
    const res = await pengine.query(`max_shootable_block(${finalGridS}, Max)`);
    if (res) {
      const newMax = res['Max'];
      if (newMax > maxShooteable) {
        setMaxShooteable(newMax);
        pushNotification(`🚀 New Block Added: ${newMax}!`); // Tipo 'central' por defecto
      }
    }

    setWaiting(false);
  }

  function revealNextBlock() {
    if (isNextBlockRevealed) return;
    setIsNextBlockRevealed(true);
    setRevealProgress(0);

    const start = Date.now();
    const duration = 10000;
    const interval = 100;

    const timer = setInterval(() => {
      const elapsed = Date.now() - start;
      if (elapsed >= duration) {
        clearInterval(timer);
        setIsNextBlockRevealed(false);
        setRevealProgress(0);
      } else {
        setRevealProgress(elapsed / duration);
      }
    }, interval);
  }

  if (grid === null) return null;

  // Filtrar notificaciones por tipo para el renderizado
  const centralNotifications = notifications.filter(n => n.type === 'central');
  const leftNotifications = notifications.filter(n => n.type === 'good' || n.type === 'excellent');
  const eliminatedNotifications = notifications.filter(n => n.type === 'eliminated'); 
  const rightNotifications = notifications.filter(n => n.type === 'combo');

  return (
    <div className="game">
      {gameOver && (
        <div className="gameover-overlay">
          <div className="gameover-content">
            <h1>Game Over</h1>
            <button onClick={() => window.location.reload()}>Reiniciar</button>
          </div>
        </div>
      )}

      {/* Contenedor para Notificaciones Centrales */}
      {centralNotifications.length > 0 && (
        <div className="notification-container">
          {centralNotifications.map(({ id, msg }) => (
            <div key={id} className="notification-bubble">
              {msg}
            </div>
          ))}
        </div>
      )}

      {/* Contenedor para Notificaciones Izquierdas (Good / Excellent) */}
      {leftNotifications.length > 0 && (
        <div className="left-notifications-wrapper">
          {leftNotifications.map(({ id, msg, type }) => (
            <div key={id} className={`notification-bubble ${type}-notification-bubble`}>
              {msg}
            </div>
          ))}
        </div>
      )}

      {/* Contenedor para Notificaciones Derechas (Combo) */}
      {rightNotifications.length > 0 && (
        <div className="right-notifications-wrapper">
          {rightNotifications.map(({ id, msg, type }) => (
            <div key={id} className={`notification-bubble ${type}-notification-bubble`}>
              {msg}
            </div>
          ))}
        </div>
      )}

      {/* Nuevo Contenedor para Notificaciones de Bloques Eliminados (Izquierda Abajo) */}
      {eliminatedNotifications.length > 0 && (
        <div className="left-down-notifications-wrapper"> {/* Nueva clase para el contenedor */}
          {eliminatedNotifications.map(({ id, msg, type }) => (
            <div 
              key={id}
              className="notification-bubble" // Mantén la clase base
               style={type === 'eliminated' ? { backgroundColor: '#e74c3c', border: '2px solid #c0392b' } : {}}
            >
              {msg}
            </div>
          ))}
        </div>
      )}

      <div className="header">
        <div className="score">{score}</div>
      </div>

      <Board
        grid={grid}
        numOfColumns={numOfColumns!}
        onLaneClick={handleLaneClick}
        fusionGroup={fusionGroup}
      />

      <div className="footer">
        <div className="blockShoot">
          {shootBlock !== null && <Block value={shootBlock} position={[0, 0]} />}
          {nextBlock !== null &&
            <div className={`next-block ${animateNextBlock && isNextBlockRevealed ? 'slide-to-left' : ''}`}>
              <div className="next-block-wrapper" onClick={revealNextBlock}>
                {isNextBlockRevealed ? (
                  <>
                    <Block value={nextBlock} position={[0, 1]} skipLaunch />
                    <div className="progress-bar">
                      <div className="progress-bar-fill" style={{ width: `${revealProgress * 100}%` }} />
                    </div>
                  </>
                ) : (
                  <div className="overlay">Bloque siguiente</div>
                )}
              </div>
            </div>}
        </div>
      </div>
    </div>
  );
}

export default Game;