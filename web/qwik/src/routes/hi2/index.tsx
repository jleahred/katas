import { component$ } from '@builder.io/qwik';
import { useLocation, useNavigate } from '@builder.io/qwik-city';
import { RequestHandler, routeLoader$ } from '@builder.io/qwik-city';



// export const onGet: RequestHandler = async ({ query, json }) => {
//   // Obtener el parámetro de consulta `name`
//   const name = query.get('name') || 'Mundo';

//   // Crear una respuesta JSON con los datos
//   const data = { message: `Hola, ${name}!` };

//   // Devolver la respuesta JSON con el código de estado 200
//   json(200, component$());
// };

// export const onRequest: RequestHandler = async ({ next, url }) => {
//   console.log('Before request', url);
//   await next();
//   console.log('After request', url);
// };

export const onRequest: RequestHandler = async (ev) => {
  console.log('Before request', ev);
  console.log('params', ev.query.get('aaa'));
  console.log('cookie', ev.cookie);
  ev.sharedMap.set("zzzzzz", "aaaaa");
  console.log('env', ev.sharedMap);
  console.log('env', ev.query);
  console.log('params', ev.query);
  ev.sharedMap.set("params", ev.query);
  console.log("111111111111111111111111111111111111");
  await ev.next();

};

export const dataFromLoading = routeLoader$(async (requestEvent) => {
  // This code runs only on the server, after every navigation
  console.log("2222222222222222222222222");
  console.log(requestEvent.sharedMap.keys());
  console.log("params...................:", requestEvent.sharedMap.get("params").get("search"))
  return { search: requestEvent.sharedMap.get("params").get("search") || "33" };
});

// export const useRouteLoader = routeLoader$((requestEvent) => {
//   console.log(requestEvent.params);
//   //Other code
// });

export default component$(() => {
  const loc = useLocation();
  console.log(JSON.stringify(loc, null, 2));
  const nav = useNavigate();
  const data = dataFromLoading();

  //const queryParams = new URLSearchParams(location.qu);

  return (
    <div>
      New route works.
      <button onClick$={async () => {
        console.log("aaa");
        console.log("bbb");
        console.log(JSON.stringify(window.location));
        await nav("#db");
      }}>
        aaa
      </button>
      <pre>
        {JSON.stringify(loc, null, 2)}

        ...
        ____
        {data.value.search}
      </pre>

      aaaaaa
    </div>
  );
});
