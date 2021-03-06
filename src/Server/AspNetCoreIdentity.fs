module AspNetCoreIdentity

open Shared

open Giraffe

open Microsoft.AspNetCore.Identity
open Microsoft.AspNetCore.Http
open Microsoft.EntityFrameworkCore
open FSharp.Control.Tasks
open System.Security.Claims
open System.Text

// define custom claims
module CustomClaims =
    let LoginMethod = "LoginMethod"
    let IsUsernameSet = "IsUsernameSet"

// define LoginMethods for custom claim "LoginMethod"
module LoginMethods =
    let LocalAuthority = "LOCAL AUTHORITY"
    let Github = "GitHub"
    let Google = "Google"

let showErrors (errors : IdentityError seq) =
    errors
    |> Seq.fold (fun acc err ->
        sprintf "Code: %s, Description: %s" err.Code err.Description
        |> acc.AppendLine : StringBuilder) (StringBuilder(""))
    |> (fun x -> x.ToString())

/// returns origin of user account (think: oauth or local authority)
let checkAccountOrigin (ctx: HttpContext) =
    let checkLocalAuth = ctx.User.HasClaim (fun c -> c = Claim(CustomClaims.LoginMethod,LoginMethods.LocalAuthority))
    if checkLocalAuth = true
    then LoginMethods.LocalAuthority
    else
        ctx.User.Claims
        |> Seq.map (fun c -> printfn "%s" c.Issuer; c.Issuer)
        |> Seq.filter (fun x -> x <> LoginMethods.LocalAuthority)
        |> Seq.groupBy (fun x -> x)
        |> fun x -> if Seq.length x > 1 then failwith "Unknown Claims issuer!" else x |> (Seq.head >> fst)

open Microsoft.AspNetCore.Authentication

let dotnetLogin (user:LoginModel) (contextPre: HttpContext) =
    let prevSignOut (ctx: HttpContext) =
        task {
            do! ctx.SignOutAsync("Cookies")
            return ctx
        } |> fun x -> x.Result
    let context = prevSignOut contextPre
    match user.Username,user.Password with
    | "","" -> LoginFail "Password and Username are empty"
    | "",_ -> LoginFail "Username is empty"
    | _,"" -> LoginFail "Password is empty"
    | _,_ ->
        task {
            let signInManager = context.GetService<SignInManager<IdentityUser>>()
            let! result = signInManager.PasswordSignInAsync(user.Username, user.Password, true, false)
            match result.Succeeded with
            | true ->
               return LoginSuccess
            | false -> return LoginFail (result.ToString())
        } |> fun x -> x.Result

let dotnetDeleteAccount (loginModel:LoginModel) (context: HttpContext) =
    if context.User.HasClaim (ClaimTypes.Name,loginModel.Username)
    then
        if context.User.HasClaim(fun c -> c <> Claim(CustomClaims.LoginMethod,LoginMethods.LocalAuthority))
        then
            task {
                let userManager = context.GetService<UserManager<IdentityUser>>()
                let! user = userManager.GetUserAsync context.User
                let! deleteResult = userManager.DeleteAsync user
                match deleteResult.Succeeded with
                | true -> return DeleteSuccess
                | false -> return DeleteFail (showErrors deleteResult.Errors)
            } |> fun x -> x.Result
        else
            task {
                let signInManager = context.GetService<SignInManager<IdentityUser>>()
                let! result = signInManager.PasswordSignInAsync(loginModel.Username, loginModel.Password, true, false)
                match result.Succeeded with
                | true ->
                    let userManager = context.GetService<UserManager<IdentityUser>>()
                    let! user = userManager.GetUserAsync context.User
                    let! deleteResult = userManager.DeleteAsync user
                    match deleteResult.Succeeded with
                    | true -> return DeleteSuccess
                    | false -> return DeleteFail (showErrors deleteResult.Errors)
                | false -> return DeleteFail (result.ToString())
            } |> fun x -> x.Result
    else DeleteFail "Error 401 Access Denied"

let adminDeleteAccount (loginModel:LoginModel) (userInput:User) (context: HttpContext) =
    if context.User.HasClaim (ClaimTypes.Role,"Developer") || context.User.HasClaim (ClaimTypes.Role,"Admin") || context.User.HasClaim (ClaimTypes.Role,"Usermanager")
    then
        task {
            let signInManager = context.GetService<SignInManager<IdentityUser>>()
            let! result = signInManager.PasswordSignInAsync(loginModel.Username, loginModel.Password, true, false)
            match result.Succeeded with
            | true ->
                let userManager = context.GetService<UserManager<IdentityUser>>()
                let! findByName = userManager.FindByNameAsync(userInput.Username)
                let! findByEmail = userManager.FindByEmailAsync(userInput.Email)
                let user = if findByName = findByEmail then findByName else failwith "Username and Email do not correlate"
                let! deleteResult = userManager.DeleteAsync user
                match deleteResult.Succeeded with
                | true -> return DeleteSuccess
                | false -> return DeleteFail (showErrors deleteResult.Errors)
            | false -> return DeleteFail (result.ToString())
        } |> fun x -> x.Result
    else DeleteFail "Error 401 Access Denied"

let dotnetChangeUserParams (loginModel:LoginModel) (userParameter:UserParameters) (input:string) (context: HttpContext) =
    if context.User.HasClaim (ClaimTypes.Name,loginModel.Username)
    then
        printfn "- 1 -"
        if context.User.HasClaim (fun x -> x <> Claim(CustomClaims.LoginMethod, LoginMethods.LocalAuthority))
        then
            // change params for oauth user
            task {
                printfn "- 2 -"
                let userManager = context.GetService<UserManager<IdentityUser>>()
                let! user = userManager.GetUserAsync context.User
                let! updateResult =
                    match userParameter with
                    | Email -> userManager.SetEmailAsync(user,input)
                    | Username ->
                        printfn "- 3 -"
                        userManager.SetUserNameAsync(user,input)
                    | _ -> failwith "Parameter to change can not be recognized"
                match updateResult.Succeeded with
                | true ->
                    printfn "- 4 -"
                    let signInManager = context.GetService<SignInManager<IdentityUser>>()
                    do! signInManager.SignOutAsync()
                    let! result = signInManager.SignInAsync(user,isPersistent = true)
                    return ChangeParamSuccess
                | false ->
                    printfn "- 4 fuck -"
                    return ChangeParamFail (showErrors updateResult.Errors)
            } |> fun x -> x.Result
        else
            // change params for regular user
            task {
                let signInManager = context.GetService<SignInManager<IdentityUser>>()
                let! result = signInManager.PasswordSignInAsync(loginModel.Username, loginModel.Password, true, false)
                match result.Succeeded with
                | true ->
                    let userManager = context.GetService<UserManager<IdentityUser>>()
                    let! user = userManager.GetUserAsync context.User
                    let! updateResult =
                        match userParameter with
                        | Email -> userManager.SetEmailAsync(user,input)
                        | Username -> userManager.SetUserNameAsync(user,input)
                        | Password ->
                            let updateResult =
                                let hashPw = userManager.PasswordHasher.HashPassword(user, input)
                                user.PasswordHash <- hashPw
                            userManager.UpdateAsync(user)
                        | _ -> failwith "Parameter to change can not be recognized"
                    match updateResult.Succeeded with
                    | true ->
                        let signInManager = context.GetService<SignInManager<IdentityUser>>()
                        do! signInManager.SignOutAsync()
                        let! result = signInManager.SignInAsync(user,isPersistent = true)
                        return ChangeParamSuccess
                    | false -> return ChangeParamFail (showErrors updateResult.Errors)
                | false -> return ChangeParamFail (result.ToString())
            } |> fun x -> x.Result
    else ChangeParamFail "Error 401 Access Denied"

let adminChangeUserParams (loginModel:LoginModel) (userInput:User) (userParameter:UserParameters) (input:string) (context: HttpContext) =
    let contextUserAuthentificationLevel =
        context.User.Claims |> Seq.map (fun x -> x.Type,x.Value) |> List.ofSeq |> List.find (fun (claimType,value) -> claimType = ClaimTypes.Role) |> snd |> AuxFunctions.authentificationLevelByRole
    if context.User.HasClaim (ClaimTypes.Role,"Developer") || context.User.HasClaim (ClaimTypes.Role,"Admin") || context.User.HasClaim (ClaimTypes.Role,"UserManager")
    then
        task {
            let signInManager = context.GetService<SignInManager<IdentityUser>>()
            let! result = signInManager.PasswordSignInAsync(loginModel.Username, loginModel.Password, true, false)
            match result.Succeeded with
            | true ->
                let userManager = context.GetService<UserManager<IdentityUser>>()
                let! findByName = userManager.FindByNameAsync(userInput.Username)
                let! findByEmail = userManager.FindByEmailAsync(userInput.Email)
                let user = if findByName = findByEmail then findByName else failwith "Username and Email do not correlate"
                let! getRole = (userManager.GetClaimsAsync(user))
                let checkAuthentification =
                    if AuxFunctions.authentificationLevelByRole (string userInput.Role) >= contextUserAuthentificationLevel then failwith "Error 401 Access Denied; 01"
                let! updateResult =
                    match userParameter with
                    | Email -> userManager.SetEmailAsync(user,input)
                    | Username -> userManager.SetUserNameAsync(user,input)
                    | Password ->
                        let updateResult =
                            let hashPw = userManager.PasswordHasher.HashPassword(user, input)
                            user.PasswordHash <- hashPw
                        userManager.UpdateAsync(user)
                    | Role ->
                        let role = getRole |> Seq.map (fun x -> x.Type,x.Value) |> List.ofSeq |> List.find (fun (claimType,value) -> claimType = ClaimTypes.Role) |> snd
                        let checkNewRole =
                            if AuxFunctions.authentificationLevelByRole input >= contextUserAuthentificationLevel then failwith "Error 401 Access Denied; 02"
                        userManager.ReplaceClaimAsync(user, new Claim(ClaimTypes.Role,role), new Claim(ClaimTypes.Role, input))
                match updateResult.Succeeded with
                | true ->
                    if loginModel.Username = userInput.Username
                    then
                        let signInManager = context.GetService<SignInManager<IdentityUser>>()
                        do! signInManager.SignOutAsync()
                        let! result = signInManager.SignInAsync(user,isPersistent = true)
                        return ChangeParamSuccess
                    else return ChangeParamSuccess
                | false -> return ChangeParamFail (showErrors updateResult.Errors)
            | false -> return ChangeParamFail (result.ToString())
        } |> fun x -> x.Result
    else ChangeParamFail "Error 401 Access Denied"

let dotnetGetUser (context: HttpContext) =
        task {
            let userManager = context.GetService<UserManager<IdentityUser>>()
            let! user = userManager.GetUserAsync context.User
            let claims = (userManager.GetClaimsAsync(user)) |> fun x -> x.Result |> Seq.map (fun x -> x.Type,x.Value) |> List.ofSeq
            let extLogin =
                claims |> List.tryFind (fun (claimType,value) -> claimType = CustomClaims.IsUsernameSet)
                |> fun x -> if x.IsSome then { IsTrue = true; IsUsernameSet = bool.Parse(x.Value |> snd) } else { IsTrue = false; IsUsernameSet = true }
            let role = claims |> List.find (fun (claimType,value) -> claimType = ClaimTypes.Role) |> (snd >> Shared.AuxFunctions.stringToRoles)
            let origin = claims |> List.find (fun (claimType,value) -> claimType = CustomClaims.LoginMethod) |> snd
            return { Username = user.UserName; Email = user.Email; Role = role; AccountOrigin = origin; UniqueId = user.Id; ExtLogin = extLogin }
        } |> fun x -> x.Result

let dotnetGetUserById userId (context: HttpContext) =
    let taskFunc =
        task {
            let userManager = context.GetService<UserManager<IdentityUser>>()
            let! user = userManager.FindByIdAsync(userId)
            let userInfo =
                user
                |> fun x ->
                    let claims = userManager.GetClaimsAsync(x) |> fun x -> x.Result |> Seq.map (fun x -> x.Type,x.Value) |> List.ofSeq
                    printfn "%A" claims
                    let role = claims |> List.find (fun (claimType,value) -> claimType = ClaimTypes.Role) |> (snd >> Shared.AuxFunctions.stringToRoles)
                    let origin = claims |> List.find (fun (claimType,value) -> claimType = CustomClaims.LoginMethod) |> snd
                    let extLogin =
                        claims |> List.tryFind (fun (claimType,value) -> claimType = CustomClaims.IsUsernameSet)
                        |> fun x -> if x.IsSome then { IsTrue = true; IsUsernameSet = bool.Parse(x.Value |> snd) } else { IsTrue = false; IsUsernameSet = true }
                    {
                        Username = x.UserName;
                        Email = x.Email;
                        Role = role
                        AccountOrigin = origin
                        UniqueId = x.Id
                        ExtLogin = extLogin
                    }
            return userInfo
        }
        |> fun x -> ViewUserSuccess x.Result
    if context.User.HasClaim (ClaimTypes.NameIdentifier, userId)
    then
        taskFunc
    else
        if context.User.HasClaim (ClaimTypes.Role,"Developer") || context.User.HasClaim (ClaimTypes.Role,"Admin") || context.User.HasClaim (ClaimTypes.Role,"UserManager")
        then taskFunc
        else ViewUserFail "Incorrect authentification to view the requested user."

let dotnetGetAllUsers (context: HttpContext) =
    task {
        let userManager = context.GetService<UserManager<IdentityUser>>()
        let! userList = userManager.Users.ToArrayAsync()
        let createUser =
            userList
            |> Array.map (
                fun x ->
                    let claims = userManager.GetClaimsAsync(x) |> fun x -> x.Result |> Seq.map (fun x -> x.Type,x.Value) |> List.ofSeq
                    let role = claims |> List.find (fun (claimType,value) -> claimType = ClaimTypes.Role) |> (snd >> Shared.AuxFunctions.stringToRoles)
                    let origin = claims |> List.find (fun (claimType,value) -> claimType = CustomClaims.LoginMethod) |> snd
                    let extLogin =
                        claims |> List.tryFind (fun (claimType,value) -> claimType = ClaimTypes.NameIdentifier)
                        |> fun x -> if x.IsSome then { IsTrue = true; IsUsernameSet = bool.Parse(x.Value |> snd) } else { IsTrue = false; IsUsernameSet = true }
                    {
                        Username = x.UserName;
                        Email = x.Email;
                        Role = role
                        AccountOrigin = origin
                        UniqueId = x.Id
                        ExtLogin = extLogin
                    }
                )
        return createUser
    } |> fun x -> x.Result

let dotnetUserLogOut (context: HttpContext) =
    let prevSignOut (ctx: HttpContext) =
        task {
            do! ctx.SignOutAsync("Cookies")
            return ctx
        } |> fun x -> x.Result
    task {
        let signInManager = (prevSignOut context).GetService<SignInManager<IdentityUser>>()
        do! signInManager.SignOutAsync()
        return LogoutSuccess
    } |> fun x -> x.Result

let dotnetRegistration (registerModel:RegisterModel) (context: HttpContext) =
    task {
        let user = IdentityUser(UserName = registerModel.Username, Email = registerModel.Email)
        let userManager = context.GetService<UserManager<IdentityUser>>()
        let! result = userManager.CreateAsync(user, registerModel.Password)
        match result.Succeeded with
        | false -> return (RegisterFail (showErrors result.Errors))
        | true  ->
            let! addingClaims = userManager.AddClaimAsync(user, new Claim(ClaimTypes.Role, (string ActiveUserRoles.Guest)))
            let! addingClaims2 =
                userManager.AddClaimAsync(user, new Claim(CustomClaims.LoginMethod, LoginMethods.LocalAuthority))
            match addingClaims.Succeeded,addingClaims2.Succeeded with
            | false,false -> return (RegisterFail (showErrors result.Errors))
            | true,true  ->
                let signInManager = context.GetService<SignInManager<IdentityUser>>()
                do! signInManager.SignInAsync(user, true)
                return RegisterSuccess
            | _,_ -> return (RegisterFail (showErrors result.Errors))
    } |> fun x -> x.Result

let adminUserRegistration (registerModel:RegisterModel) (role:ActiveUserRoles) (context: HttpContext) =
    if role = Guest || role = NoRole
    then RegisterFail "Cannot create a new user with the given role."
    else
        task {
            let  user        = IdentityUser(UserName = registerModel.Username, Email = registerModel.Email)
            let  userManager = context.GetService<UserManager<IdentityUser>>()
            let! result      = userManager.CreateAsync(user, registerModel.Password)
            match result.Succeeded with
            | false -> return (RegisterFail (showErrors result.Errors))
            | true  ->
                let! addingClaims = userManager.AddClaimAsync(user, new Claim(ClaimTypes.Role, string role))
                let! addingClaims2 = userManager.AddClaimAsync(user, new Claim(CustomClaims.LoginMethod, LoginMethods.LocalAuthority))
                match addingClaims.Succeeded,addingClaims2.Succeeded with
                | false,false -> return (RegisterFail (showErrors result.Errors))
                | true,true  ->
                    return RegisterSuccess
                | _,_ -> return (RegisterFail (showErrors result.Errors))
        } |> fun x -> x.Result

open FSharp.Control.Tasks.V2.ContextInsensitive

module OAuth =

    //https://github.com/giraffe-fsharp/Giraffe/blob/master/src/Giraffe/Auth.fs#L23
    let private challenge (scheme : string) (redirectUri : string) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let signinManager = ctx.GetService<SignInManager<IdentityUser>>()
                let properties = signinManager.ConfigureExternalAuthenticationProperties(scheme, redirectUri)
                do! ctx.ChallengeAsync(
                        scheme,
                        properties
                    )
                return! next ctx
            }

    let gitHubAuth : HttpHandler =
        challenge "GitHub" "http://localhost:8080"

    let googleAuth : HttpHandler =
        challenge "Google" "http://localhost:8080"

    let orcidAuth : HttpHandler =
        challenge "Orcid" "http://localhost:8080"

    let externalLoginCallback : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let signinManager = ctx.GetService<SignInManager<IdentityUser>>()
                let uniqueIdent = ctx.User.FindFirst (fun c -> c.Type = ClaimTypes.NameIdentifier) |> fun x -> x.Value
                let issuer = checkAccountOrigin ctx
                let! signInResult = signinManager.ExternalLoginSignInAsync(issuer,uniqueIdent,true)
                match signInResult.Succeeded with
                |false ->
                    let emailClaim =
                        if ctx.User.HasClaim (fun c -> c.Type = ClaimTypes.Email)
                        then ctx.User.FindFirst (fun c -> c.Type = ClaimTypes.Email) |> fun x -> x.Value
                        else issuer + "@" + uniqueIdent
                    let user = new IdentityUser(userName = emailClaim, Email = emailClaim)
                    let userManager = ctx.GetService<UserManager<IdentityUser>>()
                    let! createResult = userManager.CreateAsync(user)
                    match createResult.Succeeded with
                            | true ->
                                let info = UserLoginInfo(issuer,uniqueIdent,issuer)
                                let! addLoginResult = userManager.AddLoginAsync(user,info)
                                match addLoginResult.Succeeded with
                                | true ->
                                    let! addClaims =
                                        userManager.AddClaimsAsync(user,[
                                                Claim(
                                                    ClaimTypes.Role,"Guest",ClaimValueTypes.String,LoginMethods.LocalAuthority
                                                );
                                                Claim(
                                                    CustomClaims.LoginMethod,issuer,ClaimValueTypes.String,LoginMethods.LocalAuthority
                                                )
                                                Claim(
                                                    CustomClaims.IsUsernameSet,"false",ClaimValueTypes.Boolean,LoginMethods.LocalAuthority
                                                )
                                        ])
                                    let! signinResult = signinManager.SignInAsync(user,true)
                                    return! next ctx
                                | false -> return failwith "ExternalLoginFail AddLogin failed"
                            | false -> return failwith "ExternalLoginFail CreateUser failed"
                | true ->
                    return! next ctx
            }

    let addUsernameToExtLoginFunc (username:string) (ctx:HttpContext) =
        task {
            let userManager = ctx.GetService<UserManager<IdentityUser>>()
            let! user = userManager.GetUserAsync(ctx.User)
            let! addUserNameResult = userManager.SetUserNameAsync(user,username)
            match addUserNameResult.Succeeded with
            | true ->
                let! changeClaimResult = userManager.ReplaceClaimAsync(user, Claim(CustomClaims.IsUsernameSet,"false"), Claim(CustomClaims.IsUsernameSet,"true"))
                match changeClaimResult.Succeeded with
                | true ->
                    let signinManager = ctx.GetService<SignInManager<IdentityUser>>()
                    let! signout = signinManager.SignOutAsync()
                    let! signin = signinManager.SignInAsync(user,true)
                    return ChangeParamSuccess
                | false ->
                    return changeClaimResult.Errors.ToString() |> ChangeParamFail
            | false ->
                return addUserNameResult.Errors.ToString() |> ChangeParamFail
        } |> fun x -> x.Result