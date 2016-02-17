# Symulacja cieczy

Projekt zaliczeniowy na przedmiot "Wirtualna rzeczywistosc i wizualizacja" na AGH. Interaktywna symulacja cieczy metodą opartą na SPH (Smoothed Particle Hydrodynamics), zaimplementowana na podstawie artykułu dostępnego pod adresem [http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.121.844&rep=rep1&type=pdf](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.121.844&rep=rep1&type=pdf).

Podczas uruchomienia ilość cząsteczek jest wyznaczana empirycznie w taki sposób, aby pojedynczy krok symulacji zajmował nie więcej niż 0.1 sekundy.

Paczka APK dla Androida: [http://student.agh.edu.pl/~mradomsk/files/fluid-android.apk](http://student.agh.edu.pl/~mradomsk/files/fluid-android.apk)
Wideo: [![youtube](http://img.youtube.com/vi/wyBZcDcbnIE/0.jpg)](http://www.youtube.com/watch?v=wyBZcDcbnIE "Symulacja cieczy")

## Kompilacja/uruchamianie

Do poprawnego skompilowania projektu potrzebne są JDK i Android SDK. Inne zależności są pobierane przez skrypt gradlew.

### PC

    # poniższa komenda kompiluje i uruchamia program
    ./gradlew desktop:run

### Android

    # po poprawnym zakończeniu plik APK dla Androida znajduje się w katalogu android/build/outputs/apk
    ./gradlew android:assemble

