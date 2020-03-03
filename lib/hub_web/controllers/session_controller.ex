defmodule HubWeb.SessionController do
  use HubWeb, :controller

  alias Hub.Accounts

  def create(conn, %{"user" => %{"email" => email, "password" => password}}) do
    case Accounts.authenticate_by_email_password(email, password) do
      {:ok, user} ->
        conn
        |> put_session(:user_id, user.id)
        |> configure_session(renew: true)
        |> render("show.json", user: user)
    end
  end

  def show(%{assigns: %{current_user: current_user}} = conn, _) do
    render(conn, "show.json", user: current_user)
  end

  def show(conn, _) do
    send_resp(conn, 404, "")
  end

  def delete(conn, _) do
    conn
    |> configure_session(drop: true)
    |> send_resp(:no_content, "")
  end
end
