# Google Sign-In setup

AstroDiary signs users in with Google and exchanges the resulting ID token for
a Firebase session, so Google users are ordinary Firebase `User`s and everything
downstream (Firestore sync, the logbook) works unchanged.

Code lives in `lib/core/providers/google_auth_providers.dart` and is surfaced in
`lib/features/auth/presentation/login_page.dart`.

## Status per platform

| Platform | Status | Blocking config |
| --- | --- | --- |
| Web | Code complete | Enable the Google provider in the Firebase console |
| Android | Code complete | See [Android](#android) below - **not usable yet** |
| Windows | Code complete | Enable the Google provider in the Firebase console |
| macOS / iOS | Code complete | Enable the Google provider + `serverClientId` |
| Linux | Not supported | `google_sign_in` has no Linux implementation |

The button is hidden automatically on Linux (`isGoogleSignInSupported`).

## Firebase console (required for every platform)

1. Open the project used by `lib/core/firebase_options.dart`. That file targets
   **`astronomy-logbook`**.
2. **Authentication -> Sign-in method -> Google -> Enable**.
3. Choose a support email.

## Web

Nothing to configure in code. `google_sign_in_web` drives the Firebase Auth
popup flow. Add your deployment domain under **Authentication -> Settings ->
Authorized domains** if you host anywhere other than the default.

## Android

Android needs three things, none of which are currently in the repo.

### 1. Register an Android OAuth client

In the Firebase console: **Project settings -> Your apps -> Android app**.

- Package name must be exactly `com.example.astrodiary` (match
  `applicationId` in `android/app/build.gradle.kts`).
- SHA-1 certificate fingerprint: get it with

  ```bash
  ./android/gradlew -p android signingReport
  ```

### 2. Replace `android/app/google-services.json`

The checked-in file is **for a different project** (`astrodiary-app`, not
`astronomy-logbook`) and **contains no OAuth clients**, so the
`com.google.gms.google-services` plugin cannot supply the client IDs that
`google_sign_in` requires on Android. Download a fresh `google-services.json`
for `astronomy-logbook` and replace it.

Without this, Android sign-in fails with a client configuration error.

### 3. Set `kGoogleServerClientId`

Android needs your project's **web** OAuth 2.0 client ID. Copy it from the
console into `lib/core/providers/google_auth_providers.dart`:

```dart
const String? kGoogleServerClientId = '<web-client-id>.apps.googleusercontent.com';
```

It is currently `null`, which is why web works but Android cannot complete the
credential exchange yet.

The Gradle plugin is already wired up (`android/settings.gradle.kts` and
`android/app/build.gradle.kts`).

## Verifying

```bash
flutter analyze          # clean
flutter test             # unit tests for the flow
flutter build web --release
```

The unit tests cover the token exchange, the platform gate and sign-out
resilience using a fake `GoogleAuthClient`. They do **not** exercise a real
Google consent screen, so end-to-end verification needs the console setup above.