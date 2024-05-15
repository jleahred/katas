import { component$, useSignal } from '@builder.io/qwik';
import { routeLoader$, server$, Form, routeAction$ } from '@builder.io/qwik-city';


export const useDadJoke = routeLoader$(async () => {
  const response = await fetch('https://icanhazdadjoke.com/', {
    headers: { Accept: 'application/json' },
  });
  return (await response.json()) as {
    id: string;
    status: number;
    joke: string;
  };
});

export const useJokeVoteAction = routeAction$((props) => {
  console.log('VOTE', props);
});

export default component$(() => {
  const isFavoriteSignal = useSignal(false);
  const counter = useSignal(0);
  // Calling our `useDadJoke` hook, will return a reactive signal to the loaded data.
  const dadJokeSignal = useDadJoke();
  const favoriteJokeAction = useJokeVoteAction();

  const serverFn = server$(async function (val) { console.log(val); return val + 5; });

  return (
    <section class="section bright">
      <p>{dadJokeSignal.value.joke}...</p>
      <Form action={favoriteJokeAction}>
        <input type="hidden" name="jokeID" value={dadJokeSignal.value.id} />
        <button name="vote" value="up">
          👍
        </button>
        <button name="vote" value="down">
          👎
        </button>
      </Form>
      <button
        onClick$={() => (isFavoriteSignal.value = !isFavoriteSignal.value)}
      >
        {isFavoriteSignal.value ? '❤️' : '🤍'}
      </button>

      <p></p>
      <button onClick$={async () => {
        // const result = await serverFn();
        // console.log(result);
        counter.value = await serverFn(counter.value);
      }}>{counter}</button>

    </section >
  );
});