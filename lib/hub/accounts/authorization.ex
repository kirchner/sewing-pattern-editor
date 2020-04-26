defmodule Hub.Accounts.Authorization do
  use Ecto.Schema
  import Ecto.Changeset

  schema "authorizations" do
    field :provider, :string
    field :uid, :string
    field :token, :string
    field :refresh_token, :string
    field :expires_at, :integer
    field :password, :string, virtual: true
    field :password_confirmation, :string, virtual: true

    belongs_to :user, Hub.Accounts.User

    timestamps()
  end

  @required_fields ~w(provider uid user_id token)a
  @optional_fields ~w(refresh_token expires_at)a

  @doc false
  def changeset(authorization, attrs) do
    authorization
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> foreign_key_constraint(:user_id)
    |> unique_constraint(:provider_uid)
  end
end
