defmodule HubWeb.Router do
  use HubWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :authenticate_user
  end

  pipeline :api do
    plug :accepts, ["json"]
    plug :fetch_session
    plug :protect_from_forgery
    plug :authenticate_user
  end

  scope "/", HubWeb do
    pipe_through :browser

    get "/", PageController, :index
    resources "/sessions", SessionController, only: [:create, :show, :delete], singleton: true
  end

  scope "/api", HubWeb do
    pipe_through :api

    resources "/users", UserController
  end

  defp authenticate_user(conn, _) do
    case get_session(conn, :user_id) do
      nil ->     conn
      user_id -> assign(conn, :current_user, Hub.Accounts.get_user!(user_id))
    end
  end
end
