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

type EffectInfoTerm = NewBlockTerm | ScoreTerm | PrologTerm;
interface NewBlockTerm extends PrologTerm {
  functor: "newBlock";
  args: [number];
}
interface ScoreTerm extends PrologTerm {
  functor: "score";
  args: [number];
}

function Game() {
  const [pengine, setPengine] = useState<any>(null);
  const [grid, setGrid] = useState<Grid | null>(null);
  const [numOfColumns, setNumOfColumns] = useState<number | null>(null);
  const [score, setScore] = useState<number>(0);
  const [shootBlock, setShootBlock] = useState<number | null>(null);
  const [waiting, setWaiting] = useState<boolean>(false);
  const [fusionGroup, setFusionGroup] = useState<number[]>([]);

  useEffect(() => { connectToPenginesServer(); }, []);
  useEffect(() => { if (pengine) initGame(); }, [pengine]);

  async function connectToPenginesServer() {
    setPengine(await PengineClient.create());
  }

  async function initGame() {
    const res = await pengine!.query('init(Grid, NumOfColumns), randomBlock(Grid, Block)');
    setGrid(res['Grid']);
    setShootBlock(res['Block']);
    setNumOfColumns(res['NumOfColumns']);
  }

  async function handleLaneClick(lane: number) {
    if (waiting) return;

    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const queryS = `shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block)`;
    setWaiting(true);

    const response = await pengine.query(queryS);
    if (response) {
      animateEffect(response['Effects']);
      setShootBlock(response['Block']);
    } else {
      setWaiting(false);
    }
  }

  async function animateEffect(effects: EffectTerm[]) {
    let prevGrid = grid;

    for (const effect of effects) {
      const [effectGrid, effectInfo] = effect.args;

      // Detectar receptor: celda que aumenta de valor
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

      setGrid(effectGrid);

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
            ((idx+1) % numOfColumns !== 0) ? idx + 1 : -1
          ];
          for (const n of neighbors) {
            if (n >=0 && n < prevGrid.length && !visited.has(n) && prevGrid[n] === targetVal) {
              visited.add(n);
              queue.push(n);
            }
          }
        }

        setFusionGroup(cluster);
        await delay(500); // mostrar animación
        setFusionGroup([]);
      }

      effectInfo.forEach(({ functor, args }) => {
        if (functor === 'newBlock' || functor === 'score') {
          setScore(s => s + args[0]);
        }
      });

      prevGrid = effectGrid;
      await delay(300);
    }
    setWaiting(false);
  }

  if (grid === null) return null;

  return (
    <div className="game">
      <div className="header">
        <div className="score">{score}</div>
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns!}
        onLaneClick={handleLaneClick}
        fusionGroup={fusionGroup}
      />
      <div className='footer'>
        <div className='blockShoot'>
          <Block value={shootBlock!} position={[0,0]} />
        </div>
      </div>
    </div>
  );
}

export default Game;
