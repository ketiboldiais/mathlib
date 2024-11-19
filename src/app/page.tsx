import { getLatestPostSummaries } from "@/utils/mdxUtils";
import Link from "next/link";

const posts = await getLatestPostSummaries();

export default function Home() {
  return (
    <div className="grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20 font-[family-name:var(--font-geist-sans)]">
      <main className="flex flex-col gap-8 row-start-2 items-center sm:items-start">
      </main>
        <div>
        <p>This is a collection of math and computer science notes.</p>
            <ul>
                {posts.map((post) => (
                    <li key={post.slug} className="border border-sky-500">
                        <Link href={`/blog/${post.slug}`}>
                            <h2>{post.title}</h2>
                            <p>{post.description}</p>
                            <p>{new Date(post.date).toDateString()}</p>
                        </Link>
                    </li>
                ))}
            </ul>
        </div>
      <footer className="row-start-3 flex gap-6 flex-wrap items-center justify-center">
      </footer>
    </div>
  );
}
