/**
 * ys-dm-final-project/app.java @YS Erko Sakhiyeva @DM David Metacarpa 5/9/22
 * Main java file for the TED Talks Recommendations.
 * Reads the csv wrangled in wrangling.R
 * Creates Entry objects for each of the Talks in the csv
 * Creates a list of topics taken from the Talks
 * Generates recommendations based on whichever topic the user chooses, sorted by sentiment (likes over views). Asks the user if they want to see more recommendations.
 */

import java.util.*;
import java.io.*;

public class app {
    static Scanner scan = new Scanner(System.in);

    public static void main(String[] args) {
        Scanner fin = null;
        try {
            fin = new Scanner(new File("datated.csv")); // begins to write the file
        } catch (Exception e) {
            System.out.println(e);
            System.exit(1);
        }

        List<Entry> Rs = new ArrayList<>();
        int index = 0;
        while (fin.hasNext()) {
            String s = fin.nextLine();
            String[] A = s.split(",");
            if (index != 0) // so the first entry isn't added
            {
                String title = A[1];
                String author = A[2].substring(1, A[2].length() - 1); // gets rid of quotation marks
                String date = A[3].substring(1, A[3].length() - 1); // gets rid of quotation marks
                int views = 0;
                try {
                    views = Integer.parseInt(A[4]);
                } catch (Exception e) {
                    views = (int) (A[4].charAt(0)) * (int) (Math.pow(10, (int) (A[4].charAt(4)))); // if views are in exponential form
                }
                int likes = Integer.parseInt(A[5]); // no need for try/catch, since we were the ones who wrangled the data
                String link = A[6].substring(1, A[6].length() - 1);
                double sentiment = Double.parseDouble(A[7]); // no need for try/catch
                List<String> topics = new ArrayList<>();
                String topic = "";
                for (int i = 8; i < A.length; i++) {
                    topic = A[i].trim();
                    topic = topic.replaceAll("\"", ""); // gets rid of quotation marks in some topics
                    if (!(topic.isBlank())) // takes care of bug with blank space as a topic
                    {
                        topics.add(topic);
                    }
                }

                Rs.add(new Entry(title, author, date, views, likes, link, sentiment, topics));
            }
            index++; // used so that the first entry is not added
        }

        List<String> topicList = generateTopics(Rs); // generates the topic list ahead of time

        fin.close();

        int op = 1; // can't be initialized as 0, because then program would never enter the do-while loop
        do {
            System.out.println("Menu: "); // displays menu
            System.out.println("1. Generate Recommendation");
            System.out.println("2. Display Topics");
            System.out.println("3. About");
            System.out.println("4. Authors");
            System.out.println("0. Exit");
            op = readInt("Select an option: "); // user input

            switch (op) {
                case 1 -> searchTopics(Rs); // search through topics
                case 2 -> System.out.println(topicList); // print out topic list
                case 3 -> System.out.println("Enter a topic name and our app will recommend TED Talks to you based on the topic! \nThe recommendations are sorted by likeability (likes over views). \nThe program will ask if you'd like to see more recommendations if there are more available in the dataset that match your request.");
                case 4 -> System.out.println("Erko Sakhiyeva and David Metacarpa, last updated 5/9/22"); //update this whenever edited
                case 0 -> System.out.println("Thanks for using the TED Talks recommendation generator!");
                default -> System.out.println("Not a valid input, please try again.");
            }
        } while (op != 0); // continues until exit condition
    }

    static int readInt(String s) // asks for an int until it gets one
    {
        int n = 0;
        boolean flag = false;
        do {
            System.out.print(s);
            String ns = scan.next();
            try {
                n = Integer.parseInt(ns);
                flag = true; // stops the loop
            } catch (Exception ex) {
                System.out.println(ex);
            } // sees if the input is actually an int
        }
        while (!flag);
        return n;
    }

    static void searchTopics(List<Entry> Rs) {
        System.out.print("Enter a topic: ");
        scan.nextLine(); // takes care of bug with nextLine()
        String topic = scan.nextLine();
        topic = topic.toLowerCase(); // all of the topics are in all lower case, so this removes accidental upper case letters
        topic = topic.trim(); // gets rid of accidental whitespace
        List<Entry> results = new ArrayList<>();
        for (Entry e : Rs) {
            if ((e.topics).contains(topic)) // loops through each of the entries to see if they contain the input topic
            {
                results.add(e);
            }
        }

        Collections.sort(results, Entry.sentCompare); // sorts them by sentiment value
        printList(results.subList(0, Math.min(results.size(), 5))); // only displays the first five

        int op = 3;
        int cur = 5; // current number of TED Talks displayed
        while (op != 0 && cur < results.size() - 1) {
            op = readInt("Would you like more recommendations? (1 - Yes, 0 - No): ");
            switch (op) {
                case 1:
                    printList(results.subList(cur + 1, Math.min(results.size(), cur + 5))); // displays the next five
                    cur += 5;
                    break;
                case 0:
                    System.out.println("Hope you enjoy the TED Talks!");
                    break;
                default:
                    System.out.println("Not a valid input, please try again.");
            }
        }

    }

    static List<String> generateTopics(List<Entry> Rs) {
        List<String> L = new ArrayList<>();
        for (Entry e : Rs) {
            for (String topic : e.topics) // loops through each of the topics in each Entry
            {
                if (!(L.contains(topic))) // only adds the topic if the list doesn't already contain it (to avoid repeats)
                {
                    L.add(topic);
                }
            }
        }
        Collections.sort(L);
        return L;
    }

    static <T> void printList(List<T> results) // used for both Strings and Entries
    {
        for (T e : results) {
            System.out.println(e);
        }
    }
}

class Entry // class for the individual TED Talks
{
    String title, author, date, link;
    int views, likes;
    double sentiment;
    List<String> topics;

    // constructor; don't need a default one, since the user will not be creating any of these objects
    public Entry(String title, String author, String date, int views, int likes, String link, double sentiment, List<String> topics) {
        this.title = title;
        this.author = author;
        this.date = date;
        this.views = views;
        this.likes = likes;
        this.link = link;
        this.sentiment = sentiment;
        this.topics = topics;
    }

    public static Comparator<Entry> sentCompare = new Comparator<>() // comparator method for comparing based on sentiment
    {
        public int compare(final Entry e1, final Entry e2) {
            return (int) ((e1.sentiment - e2.sentiment) * 100000); // so that the sentiments can be compared to each other (too small to just compare the difference)
        }
    };

    public String toString() // toString
    {
        return title + " by " + author + ". Link: " + link;
    }
}