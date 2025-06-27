import { motion } from 'framer-motion';
import { numberToColor } from './util';

export type Position = [number, number];
interface BlockProps {
    value: number;
    position: Position;
}

function Block({ value, position }: BlockProps) {
    const [row, column] = position;

    const launchOffset = 200; // px, cuanto más alto, más largo el trayecto

    return (
        <motion.div
            className="block"
            style={{
                backgroundColor: numberToColor(value),
                gridRow: row + 1,
                gridColumn: column + 1,
            }}
            initial={{ y: launchOffset, scale: 0.8, opacity: 0.8 }}
            animate={{ y: 0, scale: 1, opacity: 1 }}
            transition={{ duration: 0.25, ease: "easeOut" }}
        >
            {value === 0 ? "" : value}
        </motion.div>
    );
}

export default Block;
