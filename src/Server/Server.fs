open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Saturn
open Shared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe

open Microsoft.AspNetCore.Identity
open Microsoft.AspNetCore.Http
open Microsoft.EntityFrameworkCore
open System.Security.Claims
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Authentication
open System.Net.Http
open System.Net.Http.Headers
open System.Text.Json

open AspNetCoreIdentity

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let nice() =
    { Value = 69 }

// used during development to get all claims from currently logged in user
let userHandler (ctx: HttpContext) =
    let testing =
        ctx.User.Claims
        |> Seq.map (fun x -> sprintf "%s ->> %s <br><br>" x.Type x.Value)
        |> String.concat ""
    if ctx.User.Identity.IsAuthenticated then
        testing
    else
        "Not logged in"

let dotnetApi (context: HttpContext) = {
    dotnetLogin = fun (loginModel) -> async { return (dotnetLogin loginModel context) }
    dotnetRegister = fun (registerModel) -> async { return (dotnetRegistration registerModel context) }
    getContextClaims = fun () -> async { return (userHandler context)}
    initialCounter = fun () -> async {return { Value = 42 }}
    }

let dotnetSecureApi (context: HttpContext) = {
    getUserCounter = fun () -> async { return nice() }
    dotnetGetUser = fun () -> async { return (dotnetGetUser context)}
    dotnetGetUserById = fun (role,userId) -> async { return role,dotnetGetUserById userId context}
    dotnetUserLogOut = fun () -> async { return dotnetUserLogOut context }
    dotnetDeleteUserAccount = fun loginModel -> async {return (dotnetDeleteAccount loginModel context)}
    dotnetChangeUserParameters = fun (loginModel,userParam,input) -> async { return dotnetChangeUserParams loginModel userParam input context }
    addUsernameToExtLogin = fun (username) -> async { return OAuth.addUsernameToExtLoginFunc username context }
    }

let adminSecureApi (context: HttpContext) = {
    dotnetGetAllUsers = fun () -> async { return dotnetGetAllUsers context}
    adminRegisterUser = fun (registerModel,userRole) -> async { return adminUserRegistration registerModel userRole context}
    adminDeleteAccount = fun (loginModel,user) -> async { return adminDeleteAccount loginModel user context }
    adminChangeUserParameters = fun (loginModel,userInput,userParam,input) -> async { return adminChangeUserParams loginModel userInput userParam input context }
    }

// exmp http://localhost:8080/api/ISecuredApi/securedCounter
// https://zaid-ajaj.github.io/Fable.Remoting/
let webApp =

    let userApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromContext dotnetApi
        |> Remoting.withDiagnosticsLogger (printfn "%s")
        |> Remoting.buildHttpHandler

    let dotnetSecureApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromContext dotnetSecureApi
        |> Remoting.withDiagnosticsLogger (printfn "%s")
        |> Remoting.buildHttpHandler

    let adminSecureApi =
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromContext adminSecureApi
        |> Remoting.withDiagnosticsLogger (printfn "%s")
        |> Remoting.buildHttpHandler

    let mustBeLoggedIn : HttpHandler =
        requiresAuthentication (
            setStatusCode 401 >=> text "Access Denied"
        )

    let mustBeUserManager : HttpHandler =
        authorizeUser (
            fun u ->
                u.HasClaim (ClaimTypes.Role, "Developer")
                || u.HasClaim (ClaimTypes.Role, "Admin")
                || u.HasClaim (ClaimTypes.Role, "UserManager")
        ) (
            setStatusCode 401 >=> text "Access Denied"
        )

    router {
        /// urls for challenge against oauth login
        forward OAuthSigninPaths.googleOAuth OAuth.googleAuth
        forward OAuthSigninPaths.githubOAuth OAuth.gitHubAuth
        forward OAuthSigninPaths.orcidOAuth OAuth.orcidAuth
        /// oauth callback: creates useraccount and external user login for oauth user
        forward "/api/externalLoginCallback" (OAuth.externalLoginCallback >=> redirectTo false ("http://localhost:8080/"))
        /// extra log out url; not necessary
        forward "/api/logout" (signOut "Cookies")
        /// fable remoting apis
        forward "" userApi
        forward "" (mustBeLoggedIn >=> dotnetSecureApi)
        forward "" (mustBeLoggedIn >=> mustBeUserManager >=> adminSecureApi)
        forward "" (setStatusCode 404 >=> text "Not Found")
    }

/// Client ids and Client secrets
let testGoogleId = ""
let testGoogleSecret = ""

let testGithubId = ""
let testGithubSecret = ""

let testOrcidId = ""
let testOrcidSecret = ""

open FSharp.Control.Tasks
open Microsoft.AspNetCore.Identity.EntityFrameworkCore

///https://github.com/giraffe-fsharp/Giraffe/blob/master/samples/IdentityApp/IdentityApp/Program.fs
let configureServices (services : IServiceCollection) =

    ///////////////////////////////////////// SQL SERVER Block ////////////////////////////////////////////////////

    ///This database is created by the CSharp dummy project
    services.AddDbContext<IdentityDbContext>(
        fun options ->
            options.UseSqlServer(
                @""
            ) |> ignore
        ) |> ignore

    //
    services.AddDefaultIdentity<IdentityUser>(
        fun options ->
            options.User.RequireUniqueEmail <- true
            options.Password.RequireDigit <- false
            options.Password.RequiredLength <- 4
            options.Password.RequiredUniqueChars <- 2
            options.Password.RequireLowercase <- false
            options.Password.RequireNonAlphanumeric <- false
            options.Password.RequireUppercase <- false
        )
        .AddEntityFrameworkStores<IdentityDbContext>()
        .AddDefaultTokenProviders()
        |> ignore

    services.Configure<IdentityOptions>(
        fun (options:IdentityOptions) ->
            options.User.AllowedUserNameCharacters <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-._@+"
        ) |> ignore

    /// add OAuth authentications; always add callback link to create user accounts!
    services.AddAuthentication(
        fun options ->
            options.DefaultAuthenticateScheme <- CookieAuthenticationDefaults.AuthenticationScheme
            options.DefaultSignInScheme <- CookieAuthenticationDefaults.AuthenticationScheme
            options.DefaultChallengeScheme <- "Google"
        )
        .AddCookie(CookieAuthenticationDefaults.AuthenticationScheme,
            fun options ->
                options.LogoutPath <- PathString "/api/logout"
            )
        // source: https://www.eelcomulder.nl/2018/06/12/secure-your-giraffe-application-with-an-oauth-provider/
        // https://developers.google.com/identity/protocols/OAuth2
        .AddGoogle("Google",
            fun options ->
                options.ClientId <- testGoogleId
                options.ClientSecret <- testGoogleSecret

                let ev = options.Events

                ev.OnTicketReceived <-
                    fun ctx ->
                        let tsk = task {
                            ctx.ReturnUri <- "/api/externalLoginCallback"
                        }
                        Task.Factory.StartNew(fun () -> tsk.Result)
        )

        // source: https://github.com/SaturnFramework/Saturn/blob/master/src/Saturn.Extensions.Authorization/OAuth.fs
        // https://github.com/settings/developers
        .AddOAuth("GitHub",
            fun (options:OAuth.OAuthOptions) ->
                options.ClientId <- testGithubId
                options.ClientSecret <- testGithubSecret
                options.CallbackPath <- new PathString("/signin-github")

                options.AuthorizationEndpoint <- "https://github.com/login/oauth/authorize"
                options.TokenEndpoint <- "https://github.com/login/oauth/access_token"
                options.UserInformationEndpoint <- "https://api.github.com/user"

                options.ClaimActions.MapJsonKey(ClaimTypes.Name, "name")
                options.ClaimActions.MapJsonKey(ClaimTypes.NameIdentifier, "id")
                options.ClaimActions.MapJsonKey(ClaimTypes.Email, "email")
                options.ClaimActions.MapJsonKey(ClaimTypes.Locality, "location")

                let ev = options.Events
                ev.OnTicketReceived <-
                    fun ctx ->
                        let tsk = task {
                            ctx.ReturnUri <- "/api/externalLoginCallback"
                        }
                        Task.Factory.StartNew(fun () -> tsk.Result)
                ev.OnCreatingTicket <-
                    fun ctx ->
                      let tsk = task {
                        let req = new HttpRequestMessage(HttpMethod.Get, ctx.Options.UserInformationEndpoint)
                        req.Headers.Accept.Add(MediaTypeWithQualityHeaderValue("application/json"))
                        req.Headers.Authorization <- AuthenticationHeaderValue("Bearer", ctx.AccessToken)
                        let! (response : HttpResponseMessage) = ctx.Backchannel.SendAsync(req, HttpCompletionOption.ResponseHeadersRead, ctx.HttpContext.RequestAborted)
                        response.EnsureSuccessStatusCode () |> ignore
                        let! cnt = response.Content.ReadAsStringAsync()
                        let user = JsonDocument.Parse cnt |> fun x -> x.RootElement
                        ctx.RunClaimActions user
                      }
                      Task.Factory.StartNew(fun () -> tsk.Result)
        )
        /// source: https://members.orcid.org/api/tutorial/get-orcid-id
        .AddOAuth("Orcid",
            fun (options:OAuth.OAuthOptions) ->
                options.ClientId <- testOrcidId
                options.ClientSecret <- testOrcidSecret
                options.CallbackPath <- new PathString("/signin-orcid")
                options.Scope.Add "/authenticate"//"openid" ///"/read-limited" needs member api - 6.5k annual

                options.AuthorizationEndpoint <- "https://orcid.org/oauth/authorize"
                options.TokenEndpoint <- "https://orcid.org/oauth/token"
                /////the outcommented code needs the "openid" scope
                //options.UserInformationEndpoint <- "https://orcid.org/oauth/userinfo"

                options.ClaimActions.MapJsonKey(ClaimTypes.Name, "name")
                options.ClaimActions.MapJsonKey(ClaimTypes.NameIdentifier,"orcid")

                let ev = options.Events
                ev.OnTicketReceived <-
                    fun ctx ->
                        let tsk = task {
                            ctx.ReturnUri <- "/api/externalLoginCallback"
                        }
                        Task.Factory.StartNew(fun () -> tsk.Result)
                ev.OnCreatingTicket <-
                    fun ctx ->
                        let tsk = task {
                            let claims = ctx.TokenResponse.Response.RootElement
                            ctx.RunClaimActions claims
                        }
                        Task.Factory.StartNew(fun () -> tsk.Result)
        )
        |> ignore

    //adds authentification for normal Asp.net core identity model
    services.AddAuthentication(
        fun options ->
            options.DefaultAuthenticateScheme <- IdentityConstants.ApplicationScheme
            options.DefaultSignInScheme <- IdentityConstants.ApplicationScheme
        )
        |> ignore

    // Configure app cookie
    services.ConfigureApplicationCookie(
        fun options ->
            options.ExpireTimeSpan <- TimeSpan.FromDays 150.0
            options.SlidingExpiration <- true
        ) |> ignore

    // Enable CORS
    services.AddCors() |> ignore

    // Configure Giraffe dependencies
    services.AddGiraffe()

//https://saturnframework.org/docs/api/application/
let app =
    application {
    app_config (fun a -> a.UseAuthentication().UseAuthorization() )
    service_config configureServices
    use_router webApp
    url ("http://0.0.0.0:" + port.ToString() + "/")
    memory_cache
    use_static publicPath
    use_iis
    use_gzip
    }

run app