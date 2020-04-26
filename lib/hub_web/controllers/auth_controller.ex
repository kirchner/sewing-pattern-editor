defmodule HubWeb.AuthController do
  use HubWeb, :controller

  plug Ueberauth

  alias Ueberauth.Strategy.Helpers
  alias HubWeb.Auth


  def request(conn, _params) do
    render(conn, "request.html", %{
      callback_url: Helpers.callback_url(conn),
      layout: {HubWeb.LayoutView, "app.html"}})
  end

  def callback(%{assigns: %{ueberauth_failure: _fails}} = conn, _params) do
    conn
    |> redirect(to: "/")
  end

  def callback(%{assigns: %{ueberauth_auth: auth}} = conn, _params) do
    case UserFromAuth.find_or_create(auth) do
      {:ok, user} ->
        conn
        |> Auth.put_current_user(user)
        |> redirect(to: "/")

      {:error, :unauthorized} ->
        conn
        |> put_status(:unauthorized)
        |> halt()
        |> json(%{error: "unauthorized"})

      {:error, :not_found} ->
        conn
        |> put_status(:not_found)
        |> halt()
        |> json(%{error: "not found"})
    end
  end

  def delete(conn, _params) do
    conn
    |> Auth.drop_current_user()
    |> json(%{})
  end
end
