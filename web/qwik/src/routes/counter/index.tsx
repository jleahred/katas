import { component$, useSignal } from '@builder.io/qwik';
import { useLocation, type DocumentHead } from "@builder.io/qwik-city";



export default component$(() => {
  const loc = useLocation();
  console.log(Number(loc.url.searchParams.get("v") || "11"));
  console.log("=================")
  const counter = useSignal(Number(loc.url.searchParams.get("v") || "11"))

  return (
    <>
      <h1>Counter
      </h1>

      counter initialized from server side reading url

      <div>
        <br />
        <p></p>
        <button onClick$={() => {
          counter.value -= 1;
        }}> - </button>
        {counter.value}
        <button onClick$={() => { counter.value += 1; }}> + </button>
      </div>
    </>
  );
});

export const head: DocumentHead = {
  title: "testing qwik",
  meta: [
    {
      name: "name",
      content: "Qwik site description",
    },
  ],
};
