package com.ciczan.criteo;

import java.util.*;

public class ArrayIntersect {

    // O(n*m) [7, 8, 9] [4, 5, 6]
    public static int[] intersection(int[] arr1, int[] arr2) {

        int smaller = arr1.length < arr2.length ? arr1.length : arr2.length;

        final ArrayList<Integer> intersect = new ArrayList<>(smaller);

        int lastIndexFound = 0;

        for (int elem : arr1) {
            final int index =  indexOf(elem, lastIndexFound, arr2);

            if (-1*index == arr2.length)
                break;

            if (index >= 0) { //Found
                lastIndexFound = index;
                intersect.add(elem);
            } else {
                lastIndexFound = (-1 * index) -1;
            }
        }

        return  intersect.stream().mapToInt(Integer::intValue).toArray();
    }

    private static int indexOf(int elem, int start, int[] arr) {

        for (int ii = start; ii < arr.length; ii++) {
            if (arr[ii] == elem) return ii;
            if (arr[ii] > elem) return -1*(ii+1);
        }

        return -1*arr.length;
    }

    public static int[] intersectionStack(Integer[] arr1, Integer[] arr2) {

        ArrayDeque<Integer> leftStack = new ArrayDeque<>(Arrays.asList(arr1));
        ArrayDeque<Integer> rightStack = new ArrayDeque<>(Arrays.asList(arr2));
        LinkedList<Integer> inter = new LinkedList<>();

        while (!leftStack.isEmpty() && !rightStack.isEmpty()) {
            Integer x = leftStack.peek();
            Integer y = rightStack.peek();

            if (x.equals(y)) {
                inter.push(x);
                leftStack.pop();
                rightStack.pop();
            } else if (x < y) {
                leftStack.pop();
            } else {
                rightStack.pop();
            }
        }

        return inter.stream().mapToInt(Integer::intValue).toArray();
    }

    private static void print(String st) {
        System.out.println(st);
    }


    public static void main(String[] args) {
        int[] result = intersection(new int[] {1, 2, 3, 4, 5}, new int[] {1, 4, 5, 6});
        print(Arrays.toString(result));

        int[] result2 = intersectionStack(new Integer[] {1, 2, 3, 4, 5}, new Integer[] {1, 4, 5, 6});
        print(Arrays.toString(result));
    }


}