import { component$, useSignal, useTask$, useVisibleTask$ } from '@builder.io/qwik';
import { useLocation, useNavigate, type DocumentHead } from "@builder.io/qwik-city";

type Params = {
  v: string
};

export default component$(() => {
  const loc = useLocation();
  console.log(loc.url.searchParams);
  console.log("=================")
  const parse_counter = Number(loc.url.searchParams.get("v") || "11");
  console.log(parse_counter);
  const nc = isNaN(parse_counter) ? 12 : parse_counter;
  console.log(nc);
  const counter = useSignal(nc);

  const nav = useNavigate();
  //useVisibleTask$(async ({ track }) => {
  useTask$(async ({ track }) => {
    track(() => counter.value);
    //nav('?v=' + (counter.value));
    console.log(".....................");
    console.log(counter.value);
    const params: Params = { v: counter.value.toString() };
    console.log(params);
    const urlSP = new URLSearchParams(params)
    console.log(urlSP.toString());
    //nav('?' + (urlSP.toString()));
    if (loc.url.searchParams.size != 0) {
      nav('?' + (urlSP.toString()));
    }
    console.log("size: ", loc.url.searchParams.size);
  });


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
