// Board.tsx
import { motion } from 'framer-motion';
import Block, { Position } from './Block';
import { Grid } from './Game';

interface BoardProps {
  grid: Grid;
  numOfColumns: number;
  onLaneClick: (lane: number) => void;
  mergedIndex?: number | null;
}

function Board({ grid, numOfColumns, onLaneClick, mergedIndex }: BoardProps) {
  const numOfRows = grid.length / numOfColumns;

  return (
    <div className="board">
      <div
        className="blocks"
        style={{
          gridTemplateColumns: `repeat(${numOfColumns}, 70px)`,
          gridTemplateRows: `repeat(${numOfRows}, 70px)`
        }}>
        {Array.from({ length: numOfColumns }).map((_, i) => (
          <div
            key={`lane-${i}`}
            className="lane"
            style={{ gridColumn: i + 1, gridRow: `1 / span ${numOfRows}` }}
            onClick={() => {
              console.log("👉 Clic detectado en la columna:", i + 1);
              onLaneClick(i + 1);
            }}
          />
        ))}

        {grid.map((num, i) => {
          if (num === "-") return null;
          const pos: Position = [Math.floor(i / numOfColumns), i % numOfColumns];

          if (mergedIndex === i) {
            return (
              <motion.div
                key={i}
                initial={{ opacity: 1 }}
                animate={{ opacity: [1, 0.3, 1] }}
                transition={{ duration: 0.5, ease: "easeInOut" }}
                style={{ gridRow: pos[0]+1, gridColumn: pos[1]+1 }}>
                <Block value={num} position={pos} skipLaunch />
              </motion.div>
            );
          }

          return <Block key={i} value={num} position={pos} />;
        })}
      </div>
    </div>
  );
}

export default Board;
