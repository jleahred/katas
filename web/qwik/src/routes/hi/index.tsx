import { $, component$, useOnDocument, useOnWindow, useSignal, useStore } from '@builder.io/qwik';
import { Console } from 'console';

// Assume reusable use method that does not have access to JSX
// but needs to register event handlers.
function useMousePosition() {
  const position = useStore({ x: 0, y: 0 });
  useOnDocument(
    'mousemove',
    $((event) => {
      const { x, y } = event as MouseEvent;
      position.x = x;
      position.y = y;
    })
  );

  return position;
}

function dl() {
  const test = useSignal(22);
  useOnDocument(
    'domcontentloaded',
    $((event) => {
      console.log(event);
    }));
  return test;
}

export default component$(() => {
  const pos = useMousePosition();
  const test = dl();

  return (
    <div>
      New route works...

      <div>
        MousePosition: ({pos.x}, {pos.y})
      </div>

      {test.value}
    </div>
  );
});
