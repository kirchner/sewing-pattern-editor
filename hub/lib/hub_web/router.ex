defmodule HubWeb.Router do
  use HubWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", HubWeb do
    pipe_through :browser

    get "/", PageController, :index
    resources "/sessions", SessionController, only: [:create, :delete], singleton: true
  end

  scope "/api", HubWeb do
    pipe_through :api

    resources "/users", UserController
  end

  # Other scopes may use custom stacks.
  # scope "/api", HubWeb do
  #   pipe_through :api
  # end
end
