import { motion } from 'framer-motion';
import Block, { Position } from './Block';
import { Grid } from './Game';

interface BoardProps {
  grid: Grid;
  numOfColumns: number;
  onLaneClick: (lane: number) => void;
  fusionGroup: number[];
  fusionReceptorIndex?: number; // NUEVO: se pasa desde Game para evitar animación de disparo
}

function Board({ grid, numOfColumns, onLaneClick, fusionGroup, fusionReceptorIndex }: BoardProps) {
  const numOfRows = grid.length / numOfColumns;

  let receptorOffset = { x: 0, y: 0 };
  if (fusionGroup.length > 1) {
    const receptorIdx = fusionGroup[0];
    const receptorRow = Math.floor(receptorIdx / numOfColumns);
    const receptorCol = receptorIdx % numOfColumns;

    let sumX = 0, sumY = 0;
    fusionGroup.slice(1).forEach(neighborIdx => {
      const nRow = Math.floor(neighborIdx / numOfColumns);
      const nCol = neighborIdx % numOfColumns;
      sumX += (nCol - receptorCol);
      sumY += (nRow - receptorRow);
    });

    receptorOffset = {
      x: sumX * 5,
      y: sumY * 5
    };
  }

  return (
    <div className="board">
      <div
        className="blocks"
        style={{
          gridTemplateColumns: `repeat(${numOfColumns}, 70px)`,
          gridTemplateRows: `repeat(${numOfRows}, 70px)`
        }}
      >
        {Array.from({ length: numOfColumns }).map((_, i) => (
          <div
            key={`lane-${i}`}
            className="lane"
            style={{ gridColumn: i + 1, gridRow: `1 / span ${numOfRows}` }}
            onClick={() => onLaneClick(i + 1)}
          />
        ))}

        {grid.map((num, i) => {
          if (num === "-") return null;
          const row = Math.floor(i / numOfColumns), col = i % numOfColumns;
          const pos: Position = [row, col];

          const isInFusion = fusionGroup.includes(i);
          const isReceptor = i === fusionReceptorIndex;

          return (
            <motion.div
              key={`${row}-${col}`}
              initial={false}
              animate={
                isInFusion
                  ? (
                      isReceptor
                        ? { scale: 1.1 }
                        : {
                            x: (fusionGroup[0] % numOfColumns - col) * 20,
                            y: (Math.floor(fusionGroup[0] / numOfColumns) - row) * 20,
                            scale: 0,
                            opacity: 0
                          }
                    )
                  : { scale: 1, x: 0, y: 0, opacity: 1 }
              }
              transition={{ duration: 0.4, ease: "easeOut" }}
              style={{
                gridRow: row + 1,
                gridColumn: col + 1,
                zIndex: isReceptor ? 2 : 1,
                boxShadow: isReceptor ? '0 0 10px gold' : undefined,
                border: isReceptor ? '2px solid gold' : undefined,
                borderRadius: '8px',
                position: 'relative'
              }}
            >
              <Block
                value={Number(num)}
                position={pos}
                skipLaunch={isReceptor} // solo el receptor se salta la animación
              />
            </motion.div>
          );
        })}
      </div>
    </div>
  );
}

export default Board;
