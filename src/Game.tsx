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
  const [mergedIndex, setMergedIndex] = useState<number | null>(null);
  const [lastShotCol, setLastShotCol] = useState<number | null>(null);

  useEffect(() => {
    connectToPenginesServer();
  }, []);

  useEffect(() => {
    if (pengine) initGame();
  }, [pengine]);

  async function connectToPenginesServer() {
    setPengine(await PengineClient.create());
  }

  async function initGame() {
    const response = await pengine!.query('init(Grid, NumOfColumns), randomBlock(Grid, Block)');
    setGrid(response['Grid']);
    setShootBlock(response['Block']);
    setNumOfColumns(response['NumOfColumns']);
  }

  async function handleLaneClick(lane: number) {
    if (waiting) return;
    setLastShotCol(lane - 1);

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

      let fusionIdx: number | null = null;
      if (prevGrid) {
        for (let j = 0; j < effectGrid.length; j++) {
          const prev = prevGrid[j];
          const curr = effectGrid[j];
          if (prev !== '-' && curr !== '-' && Number(curr) > Number(prev)) {
            fusionIdx = j;
            break;
          }
        }
      }

      setGrid(effectGrid);

      if (fusionIdx !== null) {
        setMergedIndex(fusionIdx);
        await delay(1000); // pausa de 1s antes de seguir con otra combinacion
        setMergedIndex(null);
      }

      effectInfo.forEach(({ functor, args }) => {
        console.log("📦 Efecto recibido:", functor, args);
        if (functor === 'newBlock' || functor === 'score') {
          setScore(s => s + args[0]);
        }
      });

      prevGrid = effectGrid;
      await delay(300); // pausa breve entre efectos
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
        mergedIndex={mergedIndex}
      />
      <div className='footer'>
        <div className='blockShoot'>
          <Block
          value={shootBlock!}
          position={[0,0]}
          // omitimos skipLaunch aquí para que siga haciendo launch
          />
        </div>
      </div>
    </div>
  );
}

export default Game;