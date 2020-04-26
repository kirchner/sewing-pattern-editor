defmodule HubWeb.Auth do
  import Plug.Conn
  import Phoenix.Controller

  alias Hub.Accounts

  def init(opts), do: opts

  def call(conn, _opts) do
    user_id = get_session(conn, :user_id)

    user =
      cond do
        assigned = conn.assigns[:current_user] -> assigned
        true -> Accounts.get_user_from_id(user_id)
      end

    put_current_user(conn, user)
    |> logged_in_user(%{})
  end

  def logged_in_user(conn = %{assigns: %{current_user: %{}}}, _), do: conn

  def logged_in_user(conn, _opts) do
    conn
    |> put_status(:unauthorized)
    |> halt()
    |> json(%{error: "unauthorized"})
  end

  def put_current_user(conn, user) do
    conn
    |> assign(:current_user, user)
    |> put_session(:user_id, user && user.id)
    |> configure_session(renew: true)
  end

  def drop_current_user(conn) do
    conn
    |> configure_session(drop: true)
  end
end
