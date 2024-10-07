import { useRouteError } from "react-router-dom";

export default function ErrorPage() {
  const error: any = useRouteError();
  console.error(error);

  return (
    <div id="error-page">
      <h1>Oops!</h1>
      <p>Sorry, an unexpected error has occurred.</p>
      <p>
        <i>
          Error: {error.status} {error.statusText || error.message} <br />
          {error.data || ""}
        </i>
      </p>
    </div>
  );
}
