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
  const [notifications, setNotifications] = useState<Notification[]>([]);
  const [notificationCounter, setNotificationCounter] = useState(0);
  const [gameOver, setGameOver] = useState(false);

  function pushNotification(msg: string) {
    setNotificationCounter(id => {
      const newId = id + 1;
      setNotifications(prev => [...prev, { id: newId, msg }]);
      setTimeout(() => {
        setNotifications(prev => prev.filter(n => n.id !== newId));
      }, 5000);
      return newId;
    });
  }

  useEffect(() => { connectToPenginesServer(); }, []);
  useEffect(() => { if (pengine) initGame(); }, [pengine]);

  async function connectToPenginesServer() {
    setPengine(await PengineClient.create());
  }

  async function initGame() {
    const res = await pengine!.query('init(Grid, NumOfColumns), randomBlock(Grid, Block1), randomBlock(Grid,Block2)');
    setGrid(res['Grid']);
    setShootBlock(res['Block1']);
    setNextBlock(res['Block2']);
    setNumOfColumns(res['NumOfColumns']);
    setHighestBlockReached(0);
    setMaxShooteable(0);
    setNotifications([]);
    setGameOver(false);
  }

  async function handleLaneClick(lane: number) {
    if (waiting || nextBlock === null) return;

    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const queryS = `shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block)`;
    setWaiting(true);

    const response = await pengine.query(queryS);
    if (response) {
      animateEffect(response['Effects']);
      setAnimateNextBlock(true);
      await delay(300);
      setShootBlock(nextBlock);
      setNextBlock(response['Block']);
      setAnimateNextBlock(false);
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

        if (cluster.length > 2) {
          pushNotification(`🔥 Combo x ${cluster.length}`);
          if (cluster.length === 3) pushNotification('👍 Good!');
          else if (cluster.length === 4) pushNotification('✨ Excellent!');
        }
      }

      effectInfo.forEach(({ functor, args }) => {
        const val = args[0];

        if (functor === 'newBlock') {
          setScore(s => s + val);
          if (val > highestBlockReached) {
            setHighestBlockReached(val);
            pushNotification(`🎉 Nuevo Bloque Maximo Alcanzado: ${val}`);
          }
        }

        if (functor === 'score') {
          setScore(s => s + val);
        }

        if (functor === 'eliminatedBlock') {
          pushNotification(`❌ Eliminated block: ${val}`);
        }
      });

      prevGrid = effectGrid;
      await delay(300);
    }

    // Consultar nuevo maxShooteable
    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const res = await pengine.query(`max_shootable_block(${gridS}, Max)`);
    if (res) {
      const newMax = res['Max'];
      if (newMax > maxShooteable) {
        setMaxShooteable(newMax);
        pushNotification(`🚀 New Block Added: ${newMax}!`);
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

      {notifications.length > 0 && (
        <div className="notification-container">
          {notifications.map(({ id, msg }) => (
            <div key={id} className={`notification ${
              msg.includes('Combo') ? 'combo-notification' :
              msg.includes('Good!') ? 'good-notification' :
              msg.includes('Excellent!') ? 'excellent-notification' : ''
            }`}>
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
