defmodule UserFromAuth do
  @moduledoc """
  Retrieve the user information from the auth request
  """
  require Logger
  import Ecto.Query

  alias Hub.Repo
  alias Ueberauth.Auth
  alias Hub.Accounts

  def find_or_create(auth) do
    case auth_and_validate(auth) do
      {:error, :not_found} ->
        register_user_from_auth(auth)

      {:error, reason} ->
        {:error, reason}

      authorization ->
        user_from_authorization(authorization)
    end
  end


  # AUTH AND VALIDATE

  defp auth_and_validate(%Auth{provider: provider} = auth) do
    query = from a in Accounts.Authorization,
      where: a.uid == ^to_string(auth.uid) and a.provider == ^to_string(provider)

    case Repo.one(query) do
        nil ->
          {:error, :not_found}

        authorization ->
          if authorization.uid == to_string(auth.uid) do
            authorization
          else
            {:error, :uid_mismatch}
          end
      end
  end


  # REGISTER USER FROM AUTH

  defp register_user_from_auth(auth) do
    case Repo.transaction(fn -> create_user_from_auth(auth) end) do
      {:error, reason} ->
        {:error, reason}

      {:ok, response} ->
        response
    end
  end

  defp create_user_from_auth(auth) do
    user = create_user()
    create_authorization(user, auth)
    {:ok, user}
  end

  defp create_user do
    result =
      Accounts.User.changeset(%Accounts.User{})
      |> Repo.insert

    case result do
      {:error, reason} ->
        Repo.rollback(reason)

      {:ok, user} ->
        user
    end
  end

  defp create_authorization(user, %Auth{provider: provider} = auth) do
    authorization = Ecto.build_assoc(user, :authorizations)
    result = Accounts.Authorization.changeset(
      authorization,
      scrub(
        %{
          provider: to_string(provider),
          uid: to_string(auth.uid),
          token: auth.credentials.token,
          refresh_token: auth.credentials.refresh_token,
          expires_at: auth.credentials.expires_at,
        }
      )
    )
    |> Repo.insert

    case result do
      {:error, reason} ->
        Repo.rollback(reason)

      {:ok, _} ->
        :ok
    end
  end


  # USER FROM AUTHORIZATION

  defp user_from_authorization(auth) do
    case Repo.one(Ecto.assoc(auth, :user)) do
      nil ->
        {:error, :user_not_found}

      user ->
        {:ok, user}
    end
  end


  # HELPER

  defp scrub(params) do
    Enum.filter(params, fn
      {_key, val} when is_binary(val) ->
        String.trim(val) != ""

      {_key, val} when is_nil(val) ->
        false
    end)
    |> Enum.into(%{})
  end
end
