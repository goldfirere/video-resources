/* based on https://logic.puzzlebaron.com/play.php?u2=a2abc96da0870644f11583f67e091fd6

1. Shamir is 7 years old.
2. Shamir came from Ambalat.
3. Quirrel is younger than the ape that was found in Tarakan.
4. Of Ofallo and the ape that was found in Tarakan,one is cared for by Gracie and the other is 13 years old.
5. The animal that was found in Ambalat is either the 10-year-old or the animal Francine works with.
6. Ofallo isn't10 years old.
7. The ape that was found in Kendisi is older than the ape Dolly works with.

*/

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.stream.*;

public class Java
{
  // inject an element after _index_ elements into a stream
  private static <E> Stream<E> inject(int index, E injectee, Stream<E> input)
  {
    final var underlying = input.spliterator();

    int characteristics = underlying.characteristics() & ~(Spliterator.CONCURRENT | Spliterator.DISTINCT | Spliterator.SORTED);
    return StreamSupport.stream(new Spliterators.AbstractSpliterator<E>(underlying.estimateSize() + 1, characteristics) {
      private int num_already_returned = 0;
      public boolean tryAdvance(Consumer<? super E> action)
      {
        num_already_returned++;
        if(num_already_returned - 1 == index)
        {
          action.accept(injectee);
          return true;
        }
        else
        {
          return underlying.tryAdvance(action);
        }
      }
    }, false);
  }

  // compute all the permutations of an input list
  private static <E> List<List<E>> permutations(List<E> input, int index)
  {
    if(index < input.size())
    {
      E item = input.get(index);
      var sub_result = permutations(input, index + 1);

      var results = IntStream
        .range(0, input.size() - index)
        .mapToObj(index_to_insert ->
            sub_result.stream()
              .map(sub_list -> inject(index_to_insert, item, sub_list.stream()).toList())
              .toList()
          )
        .toList();

      return results.stream().flatMap(List::stream).toList();
    }
    else
    {
      return List.of(List.of());
    }
  }

  private enum Unit { UNIT }

  private static Stream<Unit> guard(boolean b)
  {
    return b ? Stream.of(Unit.UNIT) : Stream.empty();
  }

  private enum Orangutan { Merah, Ofallo, Quirrel, Shamir }
  private enum Handler { Dolly, Eva, Francine, Gracie }
  private enum Location { Ambalat, Basahan, Kendisi, Tarakan }

  private static List<Orangutan> orangutans = List.of(Orangutan.values());
  private static List<Handler> handlers = List.of(Handler.values());
  private static List<Location> locations = List.of(Location.values());
  private static List<Integer> ages = List.of(4, 7, 10, 13);

  private static class Assignment
  {
    Orangutan orangutan;
    Handler handler;
    Location location;
    int age;

    Assignment(Orangutan o, Handler h, Location l, int a)
    {
      orangutan = o;
      handler = h;
      location = l;
      age = a;
    }

    @Override
    public String toString()
    {
      return "(" + orangutan + "," + handler + "," + location + "," + age + ")";
    }
  }

  private static class Solution
  {
    List<Assignment> assignments;

    Solution(List<Orangutan> os, List<Handler> hs, List<Location> ls, List<Integer> as)
    {
      assignments = IntStream
        .range(0, 4)
        .mapToObj(index -> new Assignment(os.get(index), hs.get(index), ls.get(index), as.get(index)))
        .toList();
    }

    Assignment get(int index)
    {
      return assignments.get(index);
    }

    Stream<Assignment> stream()
    {
      return assignments.stream();
    }

    @Override
    public String toString()
    {
      return assignments.toString();
    }
  }

  public static void main(String[] args)
  {
    var all_solutions = permutations(handlers, 0).stream()
      .flatMap(hs -> permutations(locations, 0).stream()
      .flatMap(ls -> permutations(ages, 0).stream()
      .flatMap(as -> Stream.of(new Solution(orangutans, hs, ls, as))
      .flatMap(solution -> {
        var merah = solution.get(0);
        var ofallo = solution.get(1);
        var quirrel = solution.get(2);
        var shamir = solution.get(3);

         // Clue 1
        return guard(shamir.age == 7)
      .flatMap(u1 ->

         // Clue 2
         guard(shamir.location == Location.Ambalat)
      .flatMap(u2 ->

         // Clue 3
         solution.stream().filter(a -> a.location == Location.Tarakan)
      .flatMap(tarakan -> guard(quirrel.age < tarakan.age)
      .flatMap(u3 ->

          // Clue 4
          {
            BiFunction<Assignment, Assignment, Boolean> clue4 =
              (a1, a2) -> a1.handler == Handler.Gracie && a2.age == 13;

            return guard(clue4.apply(ofallo, tarakan) || clue4.apply(tarakan, ofallo))
      .flatMap(u4 -> guard(!tarakan.equals(ofallo))
      .flatMap(u5 ->

           // Clue 5
         solution.stream().filter(a -> a.location == Location.Ambalat)
      .flatMap(ambalat -> guard(ambalat.age == 10 || ambalat.handler == Handler.Francine)
      .flatMap(u6 ->

           // Clue 6
         guard(ofallo.age != 10)
      .flatMap(u7 ->

           // Clue 7
         solution.stream().filter(a -> a.location == Location.Kendisi)
       .flatMap(kendisi ->
         solution.stream().filter(a -> a.handler == Handler.Dolly)
       .flatMap(dolly ->
         guard(kendisi.age > dolly.age)
       .flatMap(u8 ->

      Stream.of(solution)
      ))))))));}))));}))))
      .toList();

    if(all_solutions.size() == 1)
    {
      System.out.println(all_solutions.get(0).toString());
    }
    else
    {
      System.out.println("Non-unique solutions: " + all_solutions);
    }
  }
}