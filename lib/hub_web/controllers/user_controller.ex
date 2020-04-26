defmodule HubWeb.UserController do
  use HubWeb, :controller

  action_fallback HubWeb.FallbackController

  def show(conn = %{assigns: %{current_user: user}}, _params) do
    render(conn, "show.json", user: user)
  end
end
