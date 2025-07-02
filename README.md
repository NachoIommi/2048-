# Proyecto: <span style="color:#e6538a">M2 Blocks</span>  
*Lógica para Cs. de la Computación – 2025*

---

## 🎮 El juego  
**M2 Blocks** (o *Merge 2 Blocks*) es un puzzle de estrategia en el que lanzas bloques numerados a una parrilla de 5×7, combinándolos para formar potencias de dos y sumar puntos.  

<p align="center">
  <img src="C:/Users/nacho/Desktop/2048-/proyecto-m2blocks-comision-2/docs/images/m2blocks-game-screenshot.png" alt="Screenshot M2 Blocks" width="300"/>
</p>

---

## 🛠️ Tecnologías  
- **Frontend**: React + TypeScript + Framer Motion  
- **Lógica**: SWI‑Prolog (módulo Pengines)  
- **Comunicación**: HTTP JSON entre React y servidor Pengines  
- **Estilos**: CSS puro (con BEM y animaciones clave)  

---

## 🔗 Conexión Lógica ↔ React  
1. **Prolog** expone predicados principales  
   - `init/2`: configura la grilla inicial  
   - `randomBlock/2`: genera bloques según rango dinámico  
   - `shoot/6`: simula caída + cascada de fusiones → devuelve `Effects`  
2. **React** realiza:  
   - `PengineClient.query(...)` para invocar Prolog  
   - Mapea cada `effect(Grid, Info)` a actualizaciones de estado  
   - **Detecta fusiones** comparando grids antes y después  
   - **Despliega animaciones** (Framer Motion) y notificaciones (“Combo x N”)  

---

## ✨ Principales funcionalidades  
| Función                         | Descripción resumida                                                                                                                                                                                                                                      |
|---------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Generación aleatoria**        | Rango dinámico de bloques según máximos alcanzados y prohibiciones acumuladas.                                                                                                                                                                           |
| **Efecto de disparo**           | Coloca bloque, detecta y anima encadenamientos de fusiones (grids intermedios → efectos visuales).                                                                                                                                                       |
| **Avisos “Combo x N”**          | React analiza el efecto: si `cluster.length ≥ 3` llama a `pushNotification("🔥 Combo x " + N)` con tipo `combo`.                                                                                                                                          |
| **Nuevo bloque máximo**         | Cada `newBlock(Value)` en efecto dispara `pushNotification("🎉 Nuevo Bloque Máximo Alcanzado: " + Value)` cuando Value ≥ 512.                                                                                                                              |
| **Eliminación de bloque**       | Cuando Prolog actualiza forbidden blocks, React recibe lista y muestra notificaciones “🚫 Bloque Eliminado: X” tipo `eliminated`.                                                                                                                          |
| **Booster Hint jugada**         | React pide a Prolog `find_possible_combos/4`, recibe sugerencias `suggestion(Lane, ComboSize)` y flota etiquetas semitransparentes sobre columnas por 5 s.                                                                                                 |
| **Booster Bloque siguiente**    | Muestra en pantalla, junto al bloque actual, el bloque siguiente y una barra de progreso de 10 s.                                                                                                                                                         |

---

## 🤝 Trabajo en equipo  
- **Comisión de hasta 3 integrantes**  
- **Reuniones semanales** para diseño de predicados y componentes React  
- **Pair‑programming** en la integración Prolog ↔ React  
- **Revisión mutua** de animaciones y notificaciones  

---

## ⚙️ Nivel de complejidad  
- **Prolog**: programación declarativa, manejo de listas, backtracking y Pengines  
- **React**: gestión avanzada de estado, hooks, animaciones encadenadas  
- **Integración**: serialización/deserialización de grids, sincronización de efectos visuales con lógica de servidor  

---

## 🚀 Servidor SWI‑Prolog Pengines  

### 1. Instalar SWI‑Prolog  
Descarga e instala desde: https://www.swi-prolog.org/Download.html  

### 2. Levantar Pengines  
```bash
cd pengines_server
swipl run.pl

### 3. Correr la aplicación
npm start

